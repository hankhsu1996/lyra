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
#include "lyra/mir/expr.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/process.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto LowerProcessKind(hir::ProcessKind hir_kind, diag::SourceSpan span)
    -> diag::Result<mir::ProcessKind> {
  switch (hir_kind) {
    case hir::ProcessKind::kInitial:
      return mir::ProcessKind::kInitial;
    case hir::ProcessKind::kAlways:
      return mir::ProcessKind::kAlways;
    case hir::ProcessKind::kFinal:
      return diag::Unsupported(
          span, diag::DiagCode::kUnsupportedProcessKindLowering,
          "`final` processes are not yet supported",
          diag::UnsupportedCategory::kFeature);
    case hir::ProcessKind::kAlwaysComb:
      return diag::Unsupported(
          span, diag::DiagCode::kUnsupportedProcessKindLowering,
          "`always_comb` processes are not yet supported",
          diag::UnsupportedCategory::kFeature);
    case hir::ProcessKind::kAlwaysLatch:
      return diag::Unsupported(
          span, diag::DiagCode::kUnsupportedProcessKindLowering,
          "`always_latch` processes are not yet supported",
          diag::UnsupportedCategory::kFeature);
    case hir::ProcessKind::kAlwaysFf:
      return diag::Unsupported(
          span, diag::DiagCode::kUnsupportedProcessKindLowering,
          "`always_ff` processes are not yet supported",
          diag::UnsupportedCategory::kFeature);
  }
  throw InternalError("LowerProcessKind: unknown HIR ProcessKind");
}

auto MakeTrueConditionExpr(
    ProceduralScopeLoweringState& proc_scope_state, mir::TypeId bit1)
    -> mir::ExprId {
  return proc_scope_state.AddExpr(
      mir::Expr{
          .data =
              mir::IntegerLiteral{
                  .value =
                      mir::IntegralConstant{
                          .value_words = {1ULL},
                          .state_words = {},
                          .width = 1,
                          .signedness = mir::Signedness::kUnsigned,
                          .state_kind = mir::IntegralStateKind::kTwoState,
                      }},
          .type = bit1,
      });
}

auto LowerInitialProcess(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state, const hir::Process& src,
    ProcessLoweringState& proc_state) -> diag::Result<mir::Process> {
  ProceduralScopeLoweringState process_scope_state;
  auto lowered = LowerStmt(
      unit_state, scope_state, proc_state, process_scope_state, src,
      src.stmts.at(src.root_stmt.value));
  if (!lowered) return std::unexpected(std::move(lowered.error()));
  const mir::StmtId root_id = process_scope_state.AddStmt(*std::move(lowered));
  process_scope_state.AddRootStmt(root_id);
  return mir::Process{
      .kind = mir::ProcessKind::kInitial,
      .root_procedural_scope = process_scope_state.Finish()};
}

auto LowerAlwaysProcess(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state, const hir::Process& src,
    ProcessLoweringState& proc_state) -> diag::Result<mir::Process> {
  ProceduralScopeLoweringState while_scope_state;
  {
    ProceduralDepthGuard guard{proc_state};
    auto lowered = LowerStmt(
        unit_state, scope_state, proc_state, while_scope_state, src,
        src.stmts.at(src.root_stmt.value));
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    const mir::StmtId source_id =
        while_scope_state.AddStmt(*std::move(lowered));
    while_scope_state.AddRootStmt(source_id);
    const mir::StmtId await_id = while_scope_state.AddStmt(
        mir::Stmt{
            .label = std::nullopt,
            .data = mir::AwaitStmt{.kind = mir::AwaitKind::kAlwaysBackedge},
            .child_procedural_scopes = {}});
    while_scope_state.AddRootStmt(await_id);
  }

  ProceduralScopeLoweringState process_scope_state;
  const mir::ExprId cond_id =
      MakeTrueConditionExpr(process_scope_state, unit_state.Builtins().bit1);
  std::vector<mir::ProceduralScope> child_scopes;
  const mir::ProceduralScopeId while_scope_id =
      AddChildProceduralScope(child_scopes, while_scope_state.Finish());
  const mir::StmtId while_stmt_id = process_scope_state.AddStmt(
      mir::Stmt{
          .label = std::nullopt,
          .data = mir::WhileStmt{.condition = cond_id, .scope = while_scope_id},
          .child_procedural_scopes = std::move(child_scopes)});
  process_scope_state.AddRootStmt(while_stmt_id);
  return mir::Process{
      .kind = mir::ProcessKind::kAlways,
      .root_procedural_scope = process_scope_state.Finish()};
}

}  // namespace

auto LowerProcess(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state, const hir::Process& src,
    TimeResolution time_resolution) -> diag::Result<mir::Process> {
  auto kind_or = LowerProcessKind(src.kind, src.span);
  if (!kind_or) return std::unexpected(std::move(kind_or.error()));

  ProcessLoweringState proc_state{time_resolution};
  if (*kind_or == mir::ProcessKind::kInitial) {
    return LowerInitialProcess(unit_state, scope_state, src, proc_state);
  }
  return LowerAlwaysProcess(unit_state, scope_state, src, proc_state);
}

}  // namespace lyra::lowering::hir_to_mir
