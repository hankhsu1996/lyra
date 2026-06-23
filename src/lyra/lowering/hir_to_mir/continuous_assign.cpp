#include "lyra/lowering/hir_to_mir/continuous_assign.hpp"

#include <expected>
#include <optional>
#include <utility>

#include "lyra/hir/continuous_assign.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/hir_to_mir/lhs_observable.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/sensitivity_wait.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/process.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

// LRM 10.3.2 (continuous assignment) and LRM 9.2.2.2.1 (always_comb) share a
// runtime mental model: re-evaluate the assignment whenever any RHS read
// changes. HIR keeps continuous assignment as a distinct scope-level node so
// source diagnostics retain provenance; at HIR -> MIR we materialise the
// runtime shape directly here as `mir::Process { kInitial, forever { lhs =
// rhs; SensitivityWaitStmt(reads); } }`. The body executes once at t=0 (the
// natural fall-through of the eternal loop) before the first wait, matching
// LRM 9.2.2.2's "evaluate at time 0" requirement for inferred sensitivity.
auto LowerContinuousAssign(
    const ClassLowerer& lowerer, WalkFrame frame, std::string name,
    const hir::ContinuousAssign& src) -> diag::Result<mir::Process> {
  mir::Block process_block;
  const mir::LocalId self_id = process_block.vars.Add(
      mir::LocalDecl{
          .name = "self", .type = frame.current_class->self_pointer_type});
  mir::Block body_block;
  const WalkFrame body_frame = frame.WithBlock(&body_block)
                                   .WithSelfBinding(self_id, frame.block_depth)
                                   .Deeper();

  const hir::StructuralScope& hir_scope = lowerer.HirScope();

  auto rhs_or = lowerer.LowerExpr(hir_scope.exprs.Get(src.rhs), body_frame);
  if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
  const mir::TypeId assign_type = (*rhs_or).type;
  const mir::ExprId rhs_id = body_block.exprs.Add(*std::move(rhs_or));

  auto lhs_or = lowerer.LowerLhsExpr(hir_scope.exprs.Get(src.lhs), body_frame);
  if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
  const mir::ExprId lhs_id = body_block.exprs.Add(*std::move(lhs_or));

  // Build `self.Services()` in the body so an observable LHS routes through
  // `Var<T>::Set` (or `Mutate` for a selector chain). The body's `self` is
  // already bound via `WithSelfBinding(self_id, ...)` above, so the self read
  // resolves through the frame the same way a process body's does.
  const mir::ExprId body_self_ref = body_block.exprs.Add(
      BuildSelfRefExpr(body_frame, frame.current_class->self_pointer_type));
  const mir::ExprId body_services_id = body_block.exprs.Add(
      mir::MakeServicesCallExpr(
          body_self_ref, lowerer.Module().Unit().builtins.services));

  const mir::Expr assign_expr = BuildObservableAssignExpr(
      lowerer.Module().Unit(), body_block, body_services_id, lhs_id, rhs_id,
      std::nullopt, assign_type, lowerer.Module().Unit().builtins.void_type);
  const mir::ExprId assign_id = body_block.exprs.Add(assign_expr);
  body_block.AppendStmt(
      mir::Stmt{
          .label = std::nullopt, .data = mir::ExprStmt{.expr = assign_id}});

  body_block.AppendStmt(
      BuildSensitivityWaitStmt(lowerer, src.sensitivity_list));

  const mir::BlockId body_scope_id =
      process_block.child_scopes.Add(std::move(body_block));
  process_block.AppendStmt(
      mir::Stmt{
          .label = std::nullopt,
          .data = mir::ForStmt{
              .init = {},
              .condition = std::nullopt,
              .step = {},
              .scope = body_scope_id}});
  return mir::Process{
      .kind = mir::ProcessKind::kInitial,
      .name = std::move(name),
      .root_block = std::move(process_block)};
}

}  // namespace lyra::lowering::hir_to_mir
