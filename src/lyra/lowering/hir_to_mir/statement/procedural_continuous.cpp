#include "lyra/lowering/hir_to_mir/statement/procedural_continuous.hpp"

#include <cstdint>
#include <expected>
#include <optional>
#include <string>
#include <utility>

#include "lyra/diag/diag_code.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/lowering/hir_to_mir/closure_builder.hpp"
#include "lyra/lowering/hir_to_mir/deferred_effect.hpp"
#include "lyra/lowering/hir_to_mir/integral_literal.hpp"
#include "lyra/lowering/hir_to_mir/sensitivity_wait.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/support/takeover_level.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// The level a takeover occupies crosses as an integral literal, the way every
// compile-time scalar crosses into a runtime entry.
auto LevelOperand(
    const mir::CompilationUnit& unit, mir::Block& block,
    support::TakeoverLevel level) -> mir::ExprId {
  return BuildIntLiteral(unit, block, static_cast<std::int64_t>(level));
}

// The capability a takeover acts on. LRM 10.6.1 admits only a whole variable
// and 10.6.2 adds a constant part-select of a net, so a descent here means the
// source named part of its target -- which this does not carry yet.
auto TakeoverTarget(
    ProcessLowerer& process, const WalkFrame& frame, const hir::Expr& target)
    -> diag::Result<mir::ExprId> {
  auto target_or = process.LowerLhsExpr(target, frame);
  if (!target_or) return std::unexpected(std::move(target_or.error()));
  if (!target_or->descent.empty()) {
    return diag::Fail(
        target.span, diag::DiagCode::kUnsupportedStatementForm,
        "taking over part of a target is not yet supported (LRM 10.6.2)");
  }
  return target_or->owner;
}

}  // namespace

auto LowerProceduralContinuousAssignStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::ProceduralContinuousAssignStmt& pca) -> diag::Result<mir::Stmt> {
  const hir::ProceduralBody& hir_proc = process.HirBody();
  UnitLowerer& unit_lowerer = process.Owner();
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  const mir::BuiltinMirTypes& builtins = unit.builtins;
  const hir::Expr& hir_target = hir_proc.exprs.Get(pca.target);

  mir::Block wrapper;
  const WalkFrame outer = frame.WithBlock(&wrapper);

  // The takeover is in effect from here, before anything evaluates: a
  // `release` reaching the target in the same time step must find it taken
  // over, and the generation this answers with is what says which evaluation
  // owns the level.
  auto outer_target = TakeoverTarget(process, outer, hir_target);
  if (!outer_target) return std::unexpected(std::move(outer_target.error()));
  const mir::ExprId begin = wrapper.exprs.Add(
      mir::MakeBeginTakeoverCallExpr(
          *outer_target, LevelOperand(unit, wrapper, pca.level),
          builtins.int_unsigned));

  ClosureBuilder closure(unit, outer);
  const mir::ExprId generation = SnapshotIntoClosure(
      unit_lowerer, outer, closure, begin, "takeover_generation");

  // The target is named again inside the evaluation rather than carried into
  // it: it is reached from the receiver, and the receiver is an ordinary
  // binding the same capture recursion brings across.
  auto inner_target = TakeoverTarget(process, closure.Frame(), hir_target);
  if (!inner_target) return std::unexpected(std::move(inner_target.error()));
  auto source_or =
      process.LowerExpr(hir_proc.exprs.Get(pca.source), closure.Frame());
  if (!source_or) return std::unexpected(std::move(source_or.error()));

  mir::Block& body = closure.Body();
  const mir::ExprId source = body.exprs.Add(*std::move(source_or));
  const mir::ExprId drive = body.exprs.Add(
      mir::MakeDriveTakeoverCallExpr(
          *inner_target, LevelOperand(unit, body, pca.level), generation,
          source, builtins.machine_bool));

  // An evaluation whose source nothing can change has no second pass to make,
  // so what is emitted is one drive. A takeover is created per execution of
  // the statement, so an evaluation left waiting on nothing would accumulate.
  if (pca.sensitivity_list.empty()) {
    body.AppendStmt(mir::ExprStmt{.expr = drive});
  } else {
    mir::Block wait_block;
    wait_block.AppendStmt(BuildValueChangeWaitStmt(
        wait_block, closure.Frame().WithBlock(&wait_block),
        process.EnclosingScopeLowerer(), pca.sensitivity_list));
    const mir::BlockId wait_scope =
        body.child_scopes.Add(std::move(wait_block));
    // Evaluating and driving is the loop's own condition, so the source is
    // re-read on every pass and the takeover stops the moment the target says
    // this evaluation no longer owns the level.
    body.AppendStmt(
        mir::ForStmt{
            .init = {}, .condition = drive, .step = {}, .scope = wait_scope});
  }

  wrapper.AppendStmt(
      mir::ExprStmt{
          .expr = wrapper.exprs.Add(
              RunCarrierDetached(process, outer, closure.BuildCoroutine()))});

  const mir::BlockId scope_id =
      frame.current_block->child_scopes.Add(std::move(wrapper));
  return mir::Stmt{
      .label = std::move(label), .data = mir::BlockStmt{.scope = scope_id}};
}

auto LowerProceduralContinuousEndStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::ProceduralContinuousEndStmt& pce) -> diag::Result<mir::Stmt> {
  const hir::ProceduralBody& hir_proc = process.HirBody();
  const mir::CompilationUnit& unit = process.Owner().Unit();
  mir::Block& block = *frame.current_block;

  auto target = TakeoverTarget(process, frame, hir_proc.exprs.Get(pce.target));
  if (!target) return std::unexpected(std::move(target.error()));
  const mir::ExprId call = block.exprs.Add(
      mir::MakeEndTakeoverCallExpr(
          *target, LevelOperand(unit, block, pce.level),
          unit.builtins.void_type));
  return mir::Stmt{
      .label = std::move(label), .data = mir::ExprStmt{.expr = call}};
}

}  // namespace lyra::lowering::hir_to_mir
