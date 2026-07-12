#include "lyra/lowering/hir_to_mir/statement/loops.hpp"

#include <expected>
#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/overloaded.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/statement/blocks.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerForStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::ForStmt& f) -> diag::Result<mir::Stmt> {
  const hir::ProceduralBody& hir_proc = process.HirBody();
  auto& block = *frame.current_block;

  std::vector<mir::ForInit> mir_init;
  mir_init.reserve(f.init.size());
  for (const auto& hinit : f.init) {
    auto item_or = std::visit(
        Overloaded{
            [&](const hir::ForInitDecl& d) -> diag::Result<mir::ForInit> {
              const auto& hir_local = hir_proc.procedural_vars.Get(d.var);
              const mir::TypeId type =
                  process.Module().TranslateType(hir_local.type);
              const mir::LocalId local_id = frame.bindings->Declare(
                  BindingOriginId::Procedural(d.var),
                  mir::LocalDecl{.name = hir_local.name, .type = type});
              process.MapProceduralVar(
                  d.var, AutomaticVarBinding{.type = type});
              mir::ExprId init_id{};
              if (d.init.has_value()) {
                auto init_or =
                    process.LowerExpr(hir_proc.exprs.Get(*d.init), frame);
                if (!init_or) {
                  return std::unexpected(std::move(init_or.error()));
                }
                init_id = block.exprs.Add(*std::move(init_or));
              } else {
                init_id = block.exprs.Add(BuildDefaultValueFromHir(
                    process.Module(), frame, hir_local.type));
              }
              return mir::ForInit{
                  mir::ForInitDecl{.induction_var = local_id, .init = init_id}};
            },
            [&](const hir::ForInitExpr& e) -> diag::Result<mir::ForInit> {
              auto expr_or =
                  process.LowerExpr(hir_proc.exprs.Get(e.expr), frame);
              if (!expr_or) {
                return std::unexpected(std::move(expr_or.error()));
              }
              return mir::ForInit{mir::ForInitExpr{
                  .expr = block.exprs.Add(*std::move(expr_or))}};
            },
        },
        hinit);
    if (!item_or) return std::unexpected(std::move(item_or.error()));
    mir_init.push_back(*std::move(item_or));
  }

  std::optional<mir::ExprId> cond_id;
  if (f.condition.has_value()) {
    auto cond_or = process.LowerExpr(hir_proc.exprs.Get(*f.condition), frame);
    if (!cond_or) {
      return std::unexpected(std::move(cond_or.error()));
    }
    cond_id = ReduceToCondition(
        block, block.exprs.Add(*std::move(cond_or)),
        process.Module().Unit().builtins.bit1);
  }

  std::vector<mir::ExprId> step_ids;
  step_ids.reserve(f.step.size());
  for (const hir::ExprId step_hid : f.step) {
    auto step_or = process.LowerExpr(hir_proc.exprs.Get(step_hid), frame);
    if (!step_or) {
      return std::unexpected(std::move(step_or.error()));
    }
    step_ids.push_back(block.exprs.Add(*std::move(step_or)));
  }

  auto body_or = LowerStmtIntoChildScope(process, frame, f.body);
  if (!body_or) {
    return std::unexpected(std::move(body_or.error()));
  }

  const mir::BlockId body_scope_id =
      frame.current_block->child_scopes.Add(std::move(*body_or));

  return mir::Stmt{
      .label = std::move(label),
      .data = mir::ForStmt{
          .init = std::move(mir_init),
          .condition = cond_id,
          .step = std::move(step_ids),
          .scope = body_scope_id,
          .break_label =
              f.break_label.has_value()
                  ? std::optional{mir::LoopLabelId{f.break_label->value}}
                  : std::nullopt}};
}

auto LowerWhileStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::WhileStmt& w) -> diag::Result<mir::Stmt> {
  auto& block = *frame.current_block;
  auto cond_or =
      process.LowerExpr(process.HirBody().exprs.Get(w.condition), frame);
  if (!cond_or) {
    return std::unexpected(std::move(cond_or.error()));
  }
  const mir::ExprId cond_id = ReduceToCondition(
      block, block.exprs.Add(*std::move(cond_or)),
      process.Module().Unit().builtins.bit1);

  auto body_or = LowerStmtIntoChildScope(process, frame, w.body);
  if (!body_or) {
    return std::unexpected(std::move(body_or.error()));
  }

  const mir::BlockId body_scope_id =
      frame.current_block->child_scopes.Add(std::move(*body_or));

  return mir::Stmt{
      .label = std::move(label),
      .data = mir::WhileStmt{.condition = cond_id, .scope = body_scope_id}};
}

auto LowerDoWhileStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::DoWhileStmt& d) -> diag::Result<mir::Stmt> {
  auto& block = *frame.current_block;
  auto body_or = LowerStmtIntoChildScope(process, frame, d.body);
  if (!body_or) {
    return std::unexpected(std::move(body_or.error()));
  }

  auto cond_or =
      process.LowerExpr(process.HirBody().exprs.Get(d.condition), frame);
  if (!cond_or) {
    return std::unexpected(std::move(cond_or.error()));
  }
  const mir::ExprId cond_id = ReduceToCondition(
      block, block.exprs.Add(*std::move(cond_or)),
      process.Module().Unit().builtins.bit1);

  const mir::BlockId body_scope_id =
      frame.current_block->child_scopes.Add(std::move(*body_or));

  return mir::Stmt{
      .label = std::move(label),
      .data = mir::DoWhileStmt{.condition = cond_id, .scope = body_scope_id}};
}

auto LowerRepeatStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::RepeatStmt& r) -> diag::Result<mir::Stmt> {
  const mir::TypeId int_type = process.Module().Unit().builtins.int32;
  const mir::TypeId bit_type = process.Module().Unit().builtins.bit1;

  mir::Block wrapper;
  const WalkFrame wrapper_frame = frame.WithBlock(&wrapper);

  auto count_or =
      process.LowerExpr(process.HirBody().exprs.Get(r.count), wrapper_frame);
  if (!count_or) {
    return std::unexpected(std::move(count_or.error()));
  }
  mir::ExprId count_expr_id = wrapper.exprs.Add(*std::move(count_or));
  if (wrapper.exprs.Get(count_expr_id).type != int_type) {
    count_expr_id = wrapper.exprs.Add(BuildValueConversion(
        process.Module().Unit(), wrapper, count_expr_id, int_type));
  }

  const mir::LocalId count_var = frame.bindings->DeclareAnonymous(
      mir::LocalDecl{.name = "_lyra_repeat_count", .type = int_type});
  wrapper.AppendStmt(
      mir::LocalDeclStmt{.target = count_var, .init = count_expr_id});

  const mir::LocalId idx_var = frame.bindings->DeclareAnonymous(
      mir::LocalDecl{.name = "_lyra_repeat_index", .type = int_type});

  const mir::ExprId zero_id = wrapper.exprs.Add(
      mir::MakeInt32Literal(process.Module().Unit().builtins.int32, 0));
  const mir::ExprId one_id = wrapper.exprs.Add(
      mir::MakeInt32Literal(process.Module().Unit().builtins.int32, 1));

  const mir::ExprId idx_ref_cond =
      wrapper.exprs.Add(mir::MakeLocalRefExpr(idx_var, int_type));
  const mir::ExprId count_ref_cond =
      wrapper.exprs.Add(mir::MakeLocalRefExpr(count_var, int_type));
  const mir::ExprId less_id = wrapper.exprs.Add(
      mir::Expr{
          .data =
              mir::BinaryExpr{
                  .op = mir::BinaryOp::kLessThan,
                  .lhs = idx_ref_cond,
                  .rhs = count_ref_cond},
          .type = bit_type});
  const mir::ExprId cond_id = ReduceToCondition(wrapper, less_id, bit_type);

  const mir::ExprId idx_ref_step =
      wrapper.exprs.Add(mir::MakeLocalRefExpr(idx_var, int_type));
  const mir::ExprId add_id = wrapper.exprs.Add(
      mir::Expr{
          .data =
              mir::BinaryExpr{
                  .op = mir::BinaryOp::kAdd,
                  .lhs = idx_ref_step,
                  .rhs = one_id},
          .type = int_type});
  const mir::ExprId step_target_id =
      wrapper.exprs.Add(mir::MakeLocalRefExpr(idx_var, int_type));
  const mir::ExprId step_id = wrapper.exprs.Add(
      mir::Expr{
          .data = mir::AssignExpr{.target = step_target_id, .value = add_id},
          .type = int_type});

  auto body_or = LowerStmtIntoChildScope(process, wrapper_frame, r.body);
  if (!body_or) {
    return std::unexpected(std::move(body_or.error()));
  }

  const mir::BlockId body_scope_id =
      wrapper.child_scopes.Add(std::move(*body_or));

  std::vector<mir::ForInit> for_init;
  for_init.emplace_back(
      mir::ForInitDecl{.induction_var = idx_var, .init = zero_id});

  wrapper.AppendStmt(
      mir::ForStmt{
          .init = std::move(for_init),
          .condition = cond_id,
          .step = {step_id},
          .scope = body_scope_id});

  const mir::BlockId wrapper_scope_id =
      frame.current_block->child_scopes.Add(std::move(wrapper));

  return mir::Stmt{
      .label = std::move(label),
      .data = mir::BlockStmt{.scope = wrapper_scope_id}};
}

auto LowerForeverStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::ForeverStmt& f) -> diag::Result<mir::Stmt> {
  auto body_or = LowerStmtIntoChildScope(process, frame, f.body);
  if (!body_or) {
    return std::unexpected(std::move(body_or.error()));
  }

  const mir::BlockId body_scope_id =
      frame.current_block->child_scopes.Add(std::move(*body_or));

  return mir::Stmt{
      .label = std::move(label),
      .data = mir::ForStmt{
          .init = {},
          .condition = std::nullopt,
          .step = {},
          .scope = body_scope_id}};
}

}  // namespace lyra::lowering::hir_to_mir
