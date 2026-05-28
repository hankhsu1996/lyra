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

// Lowers `src.root_stmt` into a forever-wrapped process. `tail_stmt`, if
// present, is appended after the lowered body inside the loop -- carries a
// SensitivityWaitStmt for always_comb / always_latch, nullopt for bare
// always / always_ff. Composes during construction; no post-hoc mutation.
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

// Builds the tail stmt that gates an always_comb / always_latch on its LRM
// 9.2.2.2.1 implicit sensitivity. Identity-shaped end-to-end -- distinct from
// mir::EventTrigger (which carries an expression for explicit `@(...)`).
auto BuildSensitivityWaitStmt(
    const StructuralScopeLoweringState& scope_state, const hir::Process& src)
    -> mir::Stmt {
  std::vector<mir::SensitivityRead> reads;
  reads.reserve(src.implicit_sensitivity_list.size());
  for (const auto& entry : src.implicit_sensitivity_list) {
    reads.push_back(
        mir::SensitivityRead{
            .ref = scope_state.TranslateStructuralVar(
                entry.ref.hops, entry.ref.var),
            .bit_range = entry.bit_range});
  }
  return mir::Stmt{
      .label = std::nullopt,
      .data = mir::SensitivityWaitStmt{.reads = std::move(reads)},
      .child_procedural_scopes = {}};
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
          BuildSensitivityWaitStmt(scope_state, src));
  }
  throw InternalError("LowerProcess: unknown HIR ProcessKind");
}

}  // namespace lyra::lowering::hir_to_mir
