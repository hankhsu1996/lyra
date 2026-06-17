#include "lyra/lowering/hir_to_mir/continuous_assign.hpp"

#include <expected>
#include <optional>
#include <utility>

#include "lyra/hir/continuous_assign.hpp"
#include "lyra/hir/structural_scope.hpp"
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
    const StructuralScopeLowerer& scope, WalkFrame frame, std::string name,
    const hir::ContinuousAssign& src) -> diag::Result<mir::Process> {
  mir::ProceduralScope process_scope;
  const mir::ProceduralVarId self_id = process_scope.AddProceduralVar(
      mir::ProceduralVarDecl{
          .name = "self", .type = scope.Module().Unit().builtins.self_pointer});
  mir::ProceduralScope body_scope;
  const WalkFrame body_frame =
      frame.WithProceduralScope(&body_scope)
          .WithSelfBinding(self_id, frame.procedural_depth)
          .Deeper();

  const hir::StructuralScope& hir_scope = scope.HirScope();

  auto rhs_or = scope.LowerExpr(hir_scope.GetExpr(src.rhs), body_frame);
  if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
  const mir::TypeId assign_type = (*rhs_or).type;
  const mir::ExprId rhs_id = body_scope.AddExpr(*std::move(rhs_or));

  auto lhs_or = scope.LowerExpr(hir_scope.GetExpr(src.lhs), body_frame);
  if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
  const mir::ExprId lhs_id = body_scope.AddExpr(*std::move(lhs_or));

  const mir::ExprId assign_id = body_scope.AddExpr(
      mir::Expr{
          .data = mir::AssignExpr{.target = lhs_id, .value = rhs_id},
          .type = assign_type});
  body_scope.AppendStmt(
      mir::Stmt{
          .label = std::nullopt, .data = mir::ExprStmt{.expr = assign_id}});

  body_scope.AppendStmt(BuildSensitivityWaitStmt(scope, src.sensitivity_list));

  const mir::ProceduralScopeId body_scope_id =
      process_scope.AddChildScope(std::move(body_scope));
  process_scope.AppendStmt(
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
      .root_procedural_scope = std::move(process_scope),
      .static_locals = {}};
}

}  // namespace lyra::lowering::hir_to_mir
