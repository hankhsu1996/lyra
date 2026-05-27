#include "lyra/lowering/hir_to_mir/lower_process.hpp"

#include <expected>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/time.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
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
  const mir::StmtId root_id = process_scope_state.AddStmt(*std::move(lowered));
  process_scope_state.AddRootStmt(root_id);
  return mir::Process{
      .kind = kind, .root_procedural_scope = process_scope_state.Finish()};
}

auto LowerAlwaysProcess(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state, const hir::Process& src,
    ProcessLoweringState& proc_state) -> diag::Result<mir::Process> {
  ProceduralScopeLoweringState body_scope_state;
  {
    ProceduralDepthGuard guard{proc_state};
    auto lowered = LowerStmt(
        unit_state, scope_state, proc_state, body_scope_state, src,
        src.stmts.at(src.root_stmt.value));
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    const mir::StmtId body_id = body_scope_state.AddStmt(*std::move(lowered));
    body_scope_state.AddRootStmt(body_id);
  }

  ProceduralScopeLoweringState process_scope_state;
  std::vector<mir::ProceduralScope> child_scopes;
  const mir::ProceduralScopeId body_scope_id =
      AddChildProceduralScope(child_scopes, body_scope_state.Finish());
  const mir::StmtId for_stmt_id = process_scope_state.AddStmt(
      mir::Stmt{
          .label = std::nullopt,
          .data =
              mir::ForStmt{
                  .init = {},
                  .condition = std::nullopt,
                  .step = {},
                  .scope = body_scope_id},
          .child_procedural_scopes = std::move(child_scopes)});
  process_scope_state.AddRootStmt(for_stmt_id);
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
      return LowerAlwaysProcess(unit_state, scope_state, src, proc_state);
    case hir::ProcessKind::kAlwaysComb:
      return diag::Unsupported(
          src.span, diag::DiagCode::kUnsupportedProcessKindLowering,
          "`always_comb` processes are not yet supported",
          diag::UnsupportedCategory::kFeature);
    case hir::ProcessKind::kAlwaysLatch:
      return diag::Unsupported(
          src.span, diag::DiagCode::kUnsupportedProcessKindLowering,
          "`always_latch` processes are not yet supported",
          diag::UnsupportedCategory::kFeature);
  }
  throw InternalError("LowerProcess: unknown HIR ProcessKind");
}

}  // namespace lyra::lowering::hir_to_mir
