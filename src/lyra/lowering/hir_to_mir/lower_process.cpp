#include "lyra/lowering/hir_to_mir/lower_process.hpp"

#include <expected>
#include <optional>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/time.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/lower_stmt.hpp"
#include "lyra/lowering/hir_to_mir/procedural_scope_helpers.hpp"
#include "lyra/lowering/hir_to_mir/sensitivity_wait.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/process.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto LowerStraightLineProcess(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state, const hir::Process& src,
    ProcessLoweringState& proc_state, mir::ProcessKind kind)
    -> diag::Result<mir::Process> {
  ProceduralScopeLoweringState process_scope_state;
  auto lowered = LowerStmt(
      unit_state, scope_state, proc_state, process_scope_state, src,
      src.stmts.at(src.root_stmt.value));
  if (!lowered) return std::unexpected(std::move(lowered.error()));
  process_scope_state.AddRootStmt(
      process_scope_state.AddStmt(*std::move(lowered)));
  return mir::Process{
      .kind = kind, .root_procedural_scope = process_scope_state.Finish()};
}

// Wraps `src.root_stmt` in a `forever` loop. `tail_stmt`, if present, is
// appended after the lowered body inside the loop -- carries the materialised
// SensitivityWaitStmt for always_comb / always_latch (LRM 9.2.2.2.1), nullopt
// for `always` / `always_ff` where the body itself carries any timing (e.g.
// an explicit `@(...)` or `@*` wrapper).
auto LowerForeverProcess(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state, const hir::Process& src,
    ProcessLoweringState& proc_state, std::optional<mir::Stmt> tail_stmt)
    -> diag::Result<mir::Process> {
  ProceduralScopeLoweringState body_scope_state;
  {
    ProceduralDepthGuard guard{proc_state};
    auto lowered = LowerStmt(
        unit_state, scope_state, proc_state, body_scope_state, src,
        src.stmts.at(src.root_stmt.value));
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    body_scope_state.AddRootStmt(body_scope_state.AddStmt(*std::move(lowered)));
    if (tail_stmt.has_value()) {
      body_scope_state.AddRootStmt(
          body_scope_state.AddStmt(*std::move(tail_stmt)));
    }
  }

  ProceduralScopeLoweringState process_scope_state;
  std::vector<mir::ProceduralScope> child_scopes;
  const mir::ProceduralScopeId body_scope_id =
      AddChildProceduralScope(child_scopes, body_scope_state.Finish());
  process_scope_state.AddRootStmt(process_scope_state.AddStmt(
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
      .root_procedural_scope = process_scope_state.Finish()};
}

}  // namespace

auto LowerProcess(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state, const hir::Process& src,
    TimeResolution time_resolution) -> diag::Result<mir::Process> {
  ProcessLoweringState proc_state{time_resolution};
  switch (src.kind) {
    case hir::ProcessKind::kInitial:
      return LowerStraightLineProcess(
          unit_state, scope_state, src, proc_state, mir::ProcessKind::kInitial);
    case hir::ProcessKind::kFinal:
      return LowerStraightLineProcess(
          unit_state, scope_state, src, proc_state, mir::ProcessKind::kFinal);
    case hir::ProcessKind::kAlways:
    case hir::ProcessKind::kAlwaysFf:
      return LowerForeverProcess(
          unit_state, scope_state, src, proc_state, std::nullopt);
    case hir::ProcessKind::kAlwaysComb:
    case hir::ProcessKind::kAlwaysLatch:
      return LowerForeverProcess(
          unit_state, scope_state, src, proc_state,
          BuildSensitivityWaitStmt(scope_state, src.implicit_sensitivity_list));
  }
  throw InternalError("LowerProcess: unknown HIR ProcessKind");
}

}  // namespace lyra::lowering::hir_to_mir
