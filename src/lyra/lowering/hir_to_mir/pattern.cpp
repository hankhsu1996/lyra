#include "lyra/lowering/hir_to_mir/pattern.hpp"

#include <array>
#include <cstddef>
#include <expected>
#include <optional>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/pattern.hpp"
#include "lyra/lowering/hir_to_mir/binding_origin.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/expression/operators.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto StructureComponentType(
    const mir::CompilationUnit& unit, mir::TypeId receiver_type,
    std::size_t field_index) -> mir::TypeId {
  const auto& ty = unit.types.Get(receiver_type);
  const auto* tp = std::get_if<mir::TupleType>(&ty.data);
  if (tp == nullptr) {
    throw InternalError(
        "StructureComponentType: pattern's receiver is not a tuple / unpacked "
        "struct");
  }
  if (field_index >= tp->elements.size()) {
    throw InternalError("StructureComponentType: field_index out of range");
  }
  return tp->elements[field_index];
}

}  // namespace

auto BuildChainElseIf(
    mir::Block& block, mir::LocalId taken_flag, mir::TypeId bit1_type,
    mir::BlockId else_scope) -> mir::IfStmt {
  const mir::ExprId flag_ref =
      block.exprs.Add(mir::MakeLocalRefExpr(taken_flag, bit1_type));
  const mir::ExprId not_taken = block.exprs.Add(
      mir::Expr{
          .data =
              mir::UnaryExpr{
                  .op = mir::UnaryOp::kLogicalNot, .operand = flag_ref},
          .type = bit1_type});
  return mir::IfStmt{
      .condition = ReduceToCondition(block, not_taken, bit1_type),
      .then_scope = else_scope,
      .else_scope = std::nullopt};
}

template <ExprLowerer Lowerer>
void EmitPatternBindings(
    Lowerer& lowerer, WalkFrame decl_frame, WalkFrame assign_frame,
    mir::ExprId receiver_id, mir::TypeId receiver_type,
    hir::PatternId pattern_id) {
  auto& unit = lowerer.Owner().Unit();
  const hir::Pattern& pattern = lowerer.HirPatterns().Get(pattern_id);
  auto& assign_block = *assign_frame.current_block;
  std::visit(
      Overloaded{
          [&](const hir::WildcardPattern&) {},
          [&](const hir::ConstantPattern&) {},
          [&](const hir::VariablePattern& v) {
            const mir::TypeId local_type =
                lowerer.Owner().TranslateType(v.type);
            const mir::LocalId local_id = assign_frame.bindings->Declare(
                BindingOriginId::Pattern(pattern_id),
                mir::LocalDecl{.name = v.name, .type = local_type});

            auto& decl_block = *decl_frame.current_block;
            const mir::ExprId default_id = decl_block.exprs.Add(
                BuildDefaultValueExpr(lowerer.Owner(), decl_frame, local_type));
            decl_block.AppendStmt(
                mir::LocalDeclStmt{.target = local_id, .init = default_id});

            const mir::ExprId target = assign_block.exprs.Add(
                mir::MakeLocalRefExpr(local_id, local_type));
            assign_block.AppendStmt(
                mir::ExprStmt{
                    .expr = assign_block.exprs.Add(
                        mir::MakeAssignExpr(target, receiver_id, local_type))});
          },
          [&](const hir::TaggedPattern& tp) {
            if (!tp.value_pattern.has_value()) return;
            const mir::TypeId component_type =
                TaggedComponentType(unit, receiver_type, tp.member_index);
            const mir::ExprId payload_id = assign_block.exprs.Add(
                mir::Expr{
                    .data =
                        mir::TaggedGetExpr{
                            .union_value = receiver_id,
                            .tag_index = tp.member_index},
                    .type = component_type});
            EmitPatternBindings(
                lowerer, decl_frame, assign_frame, payload_id, component_type,
                *tp.value_pattern);
          },
          [&](const hir::StructurePattern& sp) {
            for (const auto& [field_index, sub_pat_id] : sp.field_patterns) {
              const mir::TypeId field_type =
                  StructureComponentType(unit, receiver_type, field_index);
              const mir::ExprId field_id = assign_block.exprs.Add(
                  mir::Expr{
                      .data =
                          mir::TupleGetExpr{
                              .tuple = receiver_id, .index = field_index},
                      .type = field_type});
              EmitPatternBindings(
                  lowerer, decl_frame, assign_frame, field_id, field_type,
                  sub_pat_id);
            }
          },
      },
      pattern.data);
}

template <ExprLowerer Lowerer>
auto BuildPatternPredicate(
    Lowerer& lowerer, WalkFrame frame, mir::ExprId receiver_id,
    mir::TypeId receiver_type, hir::PatternId pattern_id)
    -> diag::Result<std::optional<mir::ExprId>> {
  auto& unit = lowerer.Owner().Unit();
  const hir::Pattern& pattern = lowerer.HirPatterns().Get(pattern_id);
  const mir::TypeId bit1_type = unit.builtins.bit1;
  auto& enc_block = *frame.current_block;
  return std::visit(
      Overloaded{
          [&](const hir::WildcardPattern&)
              -> diag::Result<std::optional<mir::ExprId>> {
            return std::optional<mir::ExprId>{};
          },
          [&](const hir::ConstantPattern& c)
              -> diag::Result<std::optional<mir::ExprId>> {
            auto lit_or =
                lowerer.LowerExpr(lowerer.HirExprs().Get(c.value), frame);
            if (!lit_or) return std::unexpected(std::move(lit_or.error()));
            const mir::ExprId lit_id = enc_block.exprs.Add(*std::move(lit_or));
            return enc_block.exprs.Add(BuildMirBinaryExpr(
                unit, enc_block, mir::BinaryOp::kEquality, receiver_id, lit_id,
                bit1_type));
          },
          [&](const hir::VariablePattern&)
              -> diag::Result<std::optional<mir::ExprId>> {
            return std::optional<mir::ExprId>{};
          },
          [&](const hir::TaggedPattern& tp)
              -> diag::Result<std::optional<mir::ExprId>> {
            const mir::ExprId tag_check = enc_block.exprs.Add(
                mir::Expr{
                    .data =
                        mir::TaggedIsExpr{
                            .union_value = receiver_id,
                            .tag_index = tp.member_index},
                    .type = bit1_type});
            if (!tp.value_pattern.has_value()) {
              return std::optional<mir::ExprId>{tag_check};
            }
            const mir::TypeId component_type =
                TaggedComponentType(unit, receiver_type, tp.member_index);
            const mir::ExprId payload_id = enc_block.exprs.Add(
                mir::Expr{
                    .data =
                        mir::TaggedGetExpr{
                            .union_value = receiver_id,
                            .tag_index = tp.member_index},
                    .type = component_type});
            auto inner_or = BuildPatternPredicate(
                lowerer, frame, payload_id, component_type, *tp.value_pattern);
            if (!inner_or) return std::unexpected(std::move(inner_or.error()));
            if (!inner_or->has_value()) {
              return std::optional<mir::ExprId>{tag_check};
            }
            const std::array<mir::ExprId, 2> tests{tag_check, **inner_or};
            return BuildMirLogicalAnd(unit, enc_block, bit1_type, tests);
          },
          [&](const hir::StructurePattern& sp)
              -> diag::Result<std::optional<mir::ExprId>> {
            std::vector<mir::ExprId> tests;
            tests.reserve(sp.field_patterns.size());
            for (const auto& [field_index, sub_pat_id] : sp.field_patterns) {
              const mir::TypeId field_type =
                  StructureComponentType(unit, receiver_type, field_index);
              const mir::ExprId field_id = enc_block.exprs.Add(
                  mir::Expr{
                      .data =
                          mir::TupleGetExpr{
                              .tuple = receiver_id, .index = field_index},
                      .type = field_type});
              auto sub_or = BuildPatternPredicate(
                  lowerer, frame, field_id, field_type, sub_pat_id);
              if (!sub_or) return std::unexpected(std::move(sub_or.error()));
              if (sub_or->has_value()) tests.push_back(**sub_or);
            }
            // A structure of patterns that each constrain nothing constrains
            // nothing itself, which an enclosing pattern must be able to see.
            if (tests.empty()) return std::optional<mir::ExprId>{};
            return BuildMirLogicalAnd(unit, enc_block, bit1_type, tests);
          },
      },
      pattern.data);
}

template void EmitPatternBindings(
    ProcessLowerer&, WalkFrame, WalkFrame, mir::ExprId, mir::TypeId,
    hir::PatternId);
template void EmitPatternBindings(
    const StructuralScopeLowerer&, WalkFrame, WalkFrame, mir::ExprId,
    mir::TypeId, hir::PatternId);
template auto BuildPatternPredicate(
    ProcessLowerer&, WalkFrame, mir::ExprId, mir::TypeId, hir::PatternId)
    -> diag::Result<std::optional<mir::ExprId>>;
template auto BuildPatternPredicate(
    const StructuralScopeLowerer&, WalkFrame, mir::ExprId, mir::TypeId,
    hir::PatternId) -> diag::Result<std::optional<mir::ExprId>>;

}  // namespace lyra::lowering::hir_to_mir
