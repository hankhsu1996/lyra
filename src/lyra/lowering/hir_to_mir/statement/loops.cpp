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
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/statement/blocks.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/conversion.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/procedural_hops.hpp"
#include "lyra/mir/procedural_var.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerForStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::ForStmt& f) -> diag::Result<mir::Stmt> {
  const hir::ProceduralBody& hir_proc = process.HirBody();
  auto& proc_scope = *frame.current_procedural_scope;

  std::vector<mir::ForInit> mir_init;
  mir_init.reserve(f.init.size());
  for (const auto& hinit : f.init) {
    auto item_or = std::visit(
        Overloaded{
            [&](const hir::ForInitDecl& d) -> diag::Result<mir::ForInit> {
              const auto& hir_local = hir_proc.procedural_vars.at(d.var.value);
              const mir::TypeId type =
                  process.Module().TranslateType(hir_local.type);
              const mir::ProceduralVarId local_id = proc_scope.AddProceduralVar(
                  mir::ProceduralVarDecl{.name = hir_local.name, .type = type});
              process.MapProceduralVar(
                  d.var,
                  AutomaticVarBinding{
                      .declaration_procedural_depth = frame.procedural_depth,
                      .var = local_id});
              mir::ExprId init_id{};
              if (d.init.has_value()) {
                auto init_or =
                    process.LowerExpr(hir_proc.exprs.at(d.init->value), frame);
                if (!init_or) {
                  return std::unexpected(std::move(init_or.error()));
                }
                init_id = proc_scope.AddExpr(*std::move(init_or));
              } else {
                init_id = proc_scope.AddExpr(
                    BuildDefaultValueExpr(process.Module(), frame, type));
              }
              return mir::ForInit{mir::ForInitDecl{
                  .induction_var =
                      mir::ProceduralVarRef{
                          .hops = mir::ProceduralHops{.value = 0},
                          .var = local_id},
                  .init = init_id}};
            },
            [&](const hir::ForInitExpr& e) -> diag::Result<mir::ForInit> {
              auto expr_or =
                  process.LowerExpr(hir_proc.exprs.at(e.expr.value), frame);
              if (!expr_or) {
                return std::unexpected(std::move(expr_or.error()));
              }
              return mir::ForInit{mir::ForInitExpr{
                  .expr = proc_scope.AddExpr(*std::move(expr_or))}};
            },
        },
        hinit);
    if (!item_or) return std::unexpected(std::move(item_or.error()));
    mir_init.push_back(*std::move(item_or));
  }

  std::optional<mir::ExprId> cond_id;
  if (f.condition.has_value()) {
    auto cond_or =
        process.LowerExpr(hir_proc.exprs.at(f.condition->value), frame);
    if (!cond_or) {
      return std::unexpected(std::move(cond_or.error()));
    }
    cond_id = proc_scope.AddExpr(*std::move(cond_or));
  }

  std::vector<mir::ExprId> step_ids;
  step_ids.reserve(f.step.size());
  for (const hir::ExprId step_hid : f.step) {
    auto step_or = process.LowerExpr(hir_proc.exprs.at(step_hid.value), frame);
    if (!step_or) {
      return std::unexpected(std::move(step_or.error()));
    }
    step_ids.push_back(proc_scope.AddExpr(*std::move(step_or)));
  }

  auto body_or = LowerStmtIntoChildScope(process, frame, f.body);
  if (!body_or) {
    return std::unexpected(std::move(body_or.error()));
  }

  const mir::ProceduralScopeId body_scope_id =
      frame.current_procedural_scope->AddChildScope(std::move(*body_or));

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
  auto& proc_scope = *frame.current_procedural_scope;
  auto cond_or =
      process.LowerExpr(process.HirBody().exprs.at(w.condition.value), frame);
  if (!cond_or) {
    return std::unexpected(std::move(cond_or.error()));
  }
  const mir::ExprId cond_id = proc_scope.AddExpr(*std::move(cond_or));

  auto body_or = LowerStmtIntoChildScope(process, frame, w.body);
  if (!body_or) {
    return std::unexpected(std::move(body_or.error()));
  }

  const mir::ProceduralScopeId body_scope_id =
      frame.current_procedural_scope->AddChildScope(std::move(*body_or));

  return mir::Stmt{
      .label = std::move(label),
      .data = mir::WhileStmt{.condition = cond_id, .scope = body_scope_id}};
}

auto LowerDoWhileStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::DoWhileStmt& d) -> diag::Result<mir::Stmt> {
  auto& proc_scope = *frame.current_procedural_scope;
  auto body_or = LowerStmtIntoChildScope(process, frame, d.body);
  if (!body_or) {
    return std::unexpected(std::move(body_or.error()));
  }

  auto cond_or =
      process.LowerExpr(process.HirBody().exprs.at(d.condition.value), frame);
  if (!cond_or) {
    return std::unexpected(std::move(cond_or.error()));
  }
  const mir::ExprId cond_id = proc_scope.AddExpr(*std::move(cond_or));

  const mir::ProceduralScopeId body_scope_id =
      frame.current_procedural_scope->AddChildScope(std::move(*body_or));

  return mir::Stmt{
      .label = std::move(label),
      .data = mir::DoWhileStmt{.condition = cond_id, .scope = body_scope_id}};
}

auto LowerRepeatStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::RepeatStmt& r) -> diag::Result<mir::Stmt> {
  const mir::TypeId int_type = process.Module().Unit().builtins.int32;
  const mir::TypeId bit_type = process.Module().Unit().builtins.bit1;

  mir::ProceduralScope wrapper;
  const WalkFrame wrapper_frame = frame.WithProceduralScope(&wrapper).Deeper();

  auto count_or = process.LowerExpr(
      process.HirBody().exprs.at(r.count.value), wrapper_frame);
  if (!count_or) {
    return std::unexpected(std::move(count_or.error()));
  }
  mir::ExprId count_expr_id = wrapper.AddExpr(*std::move(count_or));
  if (wrapper.GetExpr(count_expr_id).type != int_type) {
    count_expr_id = wrapper.AddExpr(
        mir::Expr{
            .data =
                mir::ConversionExpr{
                    .operand = count_expr_id,
                    .kind = mir::ConversionKind::kImplicit},
            .type = int_type});
  }

  const mir::ProceduralVarId count_var = wrapper.AddProceduralVar(
      mir::ProceduralVarDecl{.name = "_lyra_repeat_count", .type = int_type});

  wrapper.AppendStmt(
      mir::Stmt{
          .label = std::nullopt,
          .data = mir::ProceduralVarDeclStmt{
              .target =
                  mir::ProceduralVarRef{
                      .hops = mir::ProceduralHops{.value = 0},
                      .var = count_var},
              .init = count_expr_id}});

  const mir::ProceduralVarId idx_var = wrapper.AddProceduralVar(
      mir::ProceduralVarDecl{.name = "_lyra_repeat_index", .type = int_type});

  const mir::ExprId zero_id = wrapper.AddExpr(
      mir::MakeInt32Literal(process.Module().Unit().builtins.int32, 0));
  const mir::ExprId one_id = wrapper.AddExpr(
      mir::MakeInt32Literal(process.Module().Unit().builtins.int32, 1));

  const mir::ExprId idx_ref_cond = wrapper.AddExpr(
      mir::Expr{
          .data =
              mir::ProceduralVarRef{
                  .hops = mir::ProceduralHops{.value = 0}, .var = idx_var},
          .type = int_type});
  const mir::ExprId count_ref_cond = wrapper.AddExpr(
      mir::Expr{
          .data =
              mir::ProceduralVarRef{
                  .hops = mir::ProceduralHops{.value = 0}, .var = count_var},
          .type = int_type});
  const mir::ExprId cond_id = wrapper.AddExpr(
      mir::Expr{
          .data =
              mir::BinaryExpr{
                  .op = mir::BinaryOp::kLessThan,
                  .lhs = idx_ref_cond,
                  .rhs = count_ref_cond},
          .type = bit_type});

  const mir::ExprId idx_ref_step = wrapper.AddExpr(
      mir::Expr{
          .data =
              mir::ProceduralVarRef{
                  .hops = mir::ProceduralHops{.value = 0}, .var = idx_var},
          .type = int_type});
  const mir::ExprId add_id = wrapper.AddExpr(
      mir::Expr{
          .data =
              mir::BinaryExpr{
                  .op = mir::BinaryOp::kAdd,
                  .lhs = idx_ref_step,
                  .rhs = one_id},
          .type = int_type});
  const mir::ExprId step_target_id = wrapper.AddExpr(
      mir::Expr{
          .data =
              mir::ProceduralVarRef{
                  .hops = mir::ProceduralHops{.value = 0}, .var = idx_var},
          .type = int_type});
  const mir::ExprId step_id = wrapper.AddExpr(
      mir::Expr{
          .data = mir::AssignExpr{.target = step_target_id, .value = add_id},
          .type = int_type});

  auto body_or = LowerStmtIntoChildScope(process, wrapper_frame, r.body);
  if (!body_or) {
    return std::unexpected(std::move(body_or.error()));
  }

  const mir::ProceduralScopeId body_scope_id =
      wrapper.AddChildScope(std::move(*body_or));

  std::vector<mir::ForInit> for_init;
  for_init.emplace_back(
      mir::ForInitDecl{
          .induction_var =
              mir::ProceduralVarRef{
                  .hops = mir::ProceduralHops{.value = 0}, .var = idx_var},
          .init = zero_id});

  wrapper.AppendStmt(
      mir::Stmt{
          .label = std::nullopt,
          .data = mir::ForStmt{
              .init = std::move(for_init),
              .condition = cond_id,
              .step = {step_id},
              .scope = body_scope_id}});

  const mir::ProceduralScopeId wrapper_scope_id =
      frame.current_procedural_scope->AddChildScope(std::move(wrapper));

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

  const mir::ProceduralScopeId body_scope_id =
      frame.current_procedural_scope->AddChildScope(std::move(*body_or));

  return mir::Stmt{
      .label = std::move(label),
      .data = mir::ForStmt{
          .init = {},
          .condition = std::nullopt,
          .step = {},
          .scope = body_scope_id}};
}

}  // namespace lyra::lowering::hir_to_mir
