#include "lyra/lowering/hir_to_mir/pattern.hpp"

#include <array>
#include <cstdint>
#include <expected>
#include <optional>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/component_index.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/pattern.hpp"
#include "lyra/hir/pattern_id.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/type_id.hpp"
#include "lyra/lowering/hir_to_mir/binding_origin.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/expression/expr_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/expression/operators.hpp"
#include "lyra/lowering/hir_to_mir/expression/selects.hpp"
#include "lyra/lowering/hir_to_mir/packed_projection.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/mir/unary_op.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// Component `index` of `subject`, whose type is `subject_type`. The single step
// by which a pattern walk descends a level. Reaching a tagged union's component
// is checked against the tag (LRM 11.9), which the tag test below guards.
auto SubjectComponent(
    UnitLowerer& owner, mir::Block& block, mir::ExprId subject,
    hir::TypeId subject_type, base::ComponentIndex index) -> mir::ExprId {
  const hir::Type& ty = owner.Hir().types.Get(subject_type);
  const auto unpacked_component =
      [&](const std::vector<hir::UnpackedAggregateField>& fields,
          mir::ExprData access) -> mir::ExprId {
    if (index.value >= fields.size()) {
      throw InternalError("SubjectComponent: component index out of range");
    }
    return block.exprs.Add(
        mir::Expr{
            .data = std::move(access),
            .type = owner.TranslateType(fields[index.value].type)});
  };
  if (const auto* s = std::get_if<hir::UnpackedStructType>(&ty.data)) {
    return unpacked_component(
        s->fields, mir::TupleGetExpr{.tuple = subject, .index = index});
  }
  // Only a tagged union is destructured by a pattern (LRM 12.6): an untagged
  // one has no tag to name the component a pattern would ask for.
  if (const auto* u = std::get_if<hir::UnpackedUnionType>(&ty.data)) {
    return unpacked_component(
        u->fields,
        mir::TaggedGetExpr{.union_value = subject, .tag_index = index});
  }
  const PackedProjection projection = ProjectPackedAggregate(owner, ty.data);
  if (index.value >= projection.members.size()) {
    throw InternalError("SubjectComponent: component index out of range");
  }
  return block.exprs.Add(BuildPackedMemberRead(
      owner, block, subject, projection, index,
      owner.TranslateType(projection.members[index.value].type)));
}

// The 1-bit test that `subject`, whose type is `subject_type`, currently holds
// the component at `index`. The two representations answer it differently -- an
// unpacked union carries the tag as its own discriminant, a packed one as a run
// of the vector -- so the test is the one place a pattern walk still sees which
// representation it is standing on.
auto BuildTagTest(
    UnitLowerer& owner, mir::Block& block, mir::ExprId subject,
    hir::TypeId subject_type, base::ComponentIndex index) -> mir::Expr {
  const hir::Type& ty = owner.Hir().types.Get(subject_type);
  if (std::holds_alternative<hir::UnpackedUnionType>(ty.data)) {
    // The tag test answers with a host boolean; re-shaping that answer into a
    // 1-bit integral is a value conversion, so it is stated here rather than
    // left for a backend to insert around the test.
    const mir::TypeId bit1 = owner.Unit().builtins.bit1;
    const mir::ExprId is_tagged = block.exprs.Add(
        mir::Expr{
            .data =
                mir::TaggedIsExpr{.union_value = subject, .tag_index = index},
            .type = bit1});
    return mir::Expr{
        .data =
            mir::CallExpr{
                .callee =
                    mir::Direct{
                        .target = support::BuiltinFn::kFromBool,
                        .qualification = mir::TypeQualifier{.type = bit1}},
                .arguments = {is_tagged}},
        .type = bit1};
  }
  const PackedProjection projection = ProjectPackedAggregate(owner, ty.data);
  return block.exprs.Get(
      BuildPackedTagTest(owner, block, subject, projection, index));
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
    mir::ExprId subject, hir::PatternId pattern_id) {
  auto& owner = lowerer.Owner();
  const hir::Pattern& pattern = lowerer.HirPatterns().Get(pattern_id);
  auto& assign_block = *assign_frame.current_block;
  std::visit(
      Overloaded{
          [&](const hir::WildcardPattern&) {},
          [&](const hir::ConstantPattern&) {},
          [&](const hir::VariablePattern& v) {
            const mir::TypeId local_type =
                owner.TranslateType(pattern.subject_type);
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
                        mir::MakeAssignExpr(target, subject, local_type))});
          },
          [&](const hir::TaggedPattern& tp) {
            if (!tp.value_pattern.has_value()) return;
            EmitPatternBindings(
                lowerer, decl_frame, assign_frame,
                SubjectComponent(
                    owner, assign_block, subject, pattern.subject_type,
                    tp.member_index),
                *tp.value_pattern);
          },
          [&](const hir::StructurePattern& sp) {
            for (const auto& [field_index, sub_pat_id] : sp.field_patterns) {
              EmitPatternBindings(
                  lowerer, decl_frame, assign_frame,
                  SubjectComponent(
                      owner, assign_block, subject, pattern.subject_type,
                      base::ComponentIndex{
                          static_cast<std::uint32_t>(field_index)}),
                  sub_pat_id);
            }
          },
      },
      pattern.data);
}

template <ExprLowerer Lowerer>
auto BuildPatternPredicate(
    Lowerer& lowerer, WalkFrame frame, mir::ExprId subject,
    hir::PatternId pattern_id) -> diag::Result<std::optional<mir::ExprId>> {
  auto& owner = lowerer.Owner();
  auto& unit = owner.Unit();
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
                unit, enc_block, mir::BinaryOp::kEquality, subject, lit_id,
                bit1_type));
          },
          [&](const hir::VariablePattern&)
              -> diag::Result<std::optional<mir::ExprId>> {
            return std::optional<mir::ExprId>{};
          },
          [&](const hir::TaggedPattern& tp)
              -> diag::Result<std::optional<mir::ExprId>> {
            // The tag test guards the component read, which is checked (LRM
            // 11.9): the read is the operand LRM 11.3.5 short-circuits away
            // when the tag names another member.
            const mir::ExprId tag_check = enc_block.exprs.Add(BuildTagTest(
                owner, enc_block, subject, pattern.subject_type,
                tp.member_index));
            if (!tp.value_pattern.has_value()) {
              return std::optional<mir::ExprId>{tag_check};
            }
            auto inner_or = BuildPatternPredicate(
                lowerer, frame,
                SubjectComponent(
                    owner, enc_block, subject, pattern.subject_type,
                    tp.member_index),
                *tp.value_pattern);
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
              auto sub_or = BuildPatternPredicate(
                  lowerer, frame,
                  SubjectComponent(
                      owner, enc_block, subject, pattern.subject_type,
                      base::ComponentIndex{
                          static_cast<std::uint32_t>(field_index)}),
                  sub_pat_id);
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
    ProcessLowerer&, WalkFrame, WalkFrame, mir::ExprId, hir::PatternId);
template void EmitPatternBindings(
    const StructuralScopeLowerer&, WalkFrame, WalkFrame, mir::ExprId,
    hir::PatternId);
template auto BuildPatternPredicate(
    ProcessLowerer&, WalkFrame, mir::ExprId, hir::PatternId)
    -> diag::Result<std::optional<mir::ExprId>>;
template auto BuildPatternPredicate(
    const StructuralScopeLowerer&, WalkFrame, mir::ExprId, hir::PatternId)
    -> diag::Result<std::optional<mir::ExprId>>;

}  // namespace lyra::lowering::hir_to_mir
