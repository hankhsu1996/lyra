#include "lyra/lowering/hir_to_mir/lower_continuous_assign.hpp"

#include <expected>
#include <optional>
#include <utility>
#include <vector>

#include "lyra/hir/continuous_assign.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/hir_to_mir/lower_expr.hpp"
#include "lyra/lowering/hir_to_mir/procedural_scope_helpers.hpp"
#include "lyra/lowering/hir_to_mir/sensitivity_wait.hpp"
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
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const hir::StructuralScope& scope, const hir::ContinuousAssign& src,
    TimeResolution time_resolution) -> diag::Result<mir::Process> {
  ProcessLoweringState proc_state{time_resolution};

  ProceduralScopeLoweringState body_state;
  {
    ProceduralDepthGuard guard{proc_state};

    auto rhs_or = LowerStructuralExpr(
        unit_state, scope_state, body_state, scope, scope.GetExpr(src.rhs));
    if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
    const mir::TypeId assign_type = (*rhs_or).type;
    const mir::ExprId rhs_id = body_state.AddExpr(*std::move(rhs_or));

    auto lhs_or = LowerStructuralExpr(
        unit_state, scope_state, body_state, scope, scope.GetExpr(src.lhs));
    if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
    const mir::ExprId lhs_id = body_state.AddExpr(*std::move(lhs_or));

    const mir::ExprId assign_id = body_state.AddExpr(
        mir::Expr{
            .data = mir::AssignExpr{.target = lhs_id, .value = rhs_id},
            .type = assign_type});
    body_state.AddRootStmt(body_state.AddStmt(
        mir::Stmt{
            .label = std::nullopt,
            .data = mir::ExprStmt{.expr = assign_id},
            .child_procedural_scopes = {}}));

    body_state.AddRootStmt(body_state.AddStmt(
        BuildSensitivityWaitStmt(scope_state, src.sensitivity_list)));
  }

  ProceduralScopeLoweringState process_state;
  std::vector<mir::ProceduralScope> child_scopes;
  const mir::ProceduralScopeId body_scope_id =
      AddChildProceduralScope(child_scopes, body_state.Finish());
  process_state.AddRootStmt(process_state.AddStmt(
      mir::Stmt{
          .label = std::nullopt,
          .data =
              mir::ForStmt{
                  .init = {},
                  .condition = std::nullopt,
                  .step = {},
                  .scope = body_scope_id},
          .child_procedural_scopes = std::move(child_scopes)}));
  return mir::Process{
      .kind = mir::ProcessKind::kInitial,
      .root_procedural_scope = process_state.Finish()};
}

}  // namespace lyra::lowering::hir_to_mir
