#include "lyra/lowering/hir_to_mir/lower_process.hpp"

#include <cstdint>
#include <expected>
#include <optional>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/time.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/body_helpers.hpp"
#include "lyra/lowering/hir_to_mir/lower_stmt.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/body_hops.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/local_var.hpp"
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

auto MakeTrueConditionExpr(BodyLoweringState& body_state, mir::TypeId bit1)
    -> mir::ExprId {
  return body_state.AddExpr(
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
    const UnitLoweringState& unit_state, const ClassLoweringState& class_state,
    const hir::Process& src, ProcessLoweringState& proc_state)
    -> diag::Result<mir::Process> {
  BodyLoweringState process_body_state;
  auto lowered = LowerStmt(
      unit_state, class_state, proc_state, process_body_state, src,
      src.stmts.at(src.body.value));
  if (!lowered) return std::unexpected(std::move(lowered.error()));
  const mir::StmtId root_id = process_body_state.AddStmt(*std::move(lowered));
  process_body_state.AddRootStmt(root_id);
  return mir::Process{
      .kind = mir::ProcessKind::kInitial, .body = process_body_state.Finish()};
}

auto LowerAlwaysProcess(
    const UnitLoweringState& unit_state, const ClassLoweringState& class_state,
    const hir::Process& src, ProcessLoweringState& proc_state)
    -> diag::Result<mir::Process> {
  BodyLoweringState while_body_state;
  {
    BodyDepthGuard guard{proc_state};
    auto lowered = LowerStmt(
        unit_state, class_state, proc_state, while_body_state, src,
        src.stmts.at(src.body.value));
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    const mir::StmtId source_id = while_body_state.AddStmt(*std::move(lowered));
    while_body_state.AddRootStmt(source_id);
    const mir::StmtId await_id = while_body_state.AddStmt(
        mir::Stmt{
            .label = std::nullopt,
            .data = mir::AwaitStmt{.kind = mir::AwaitKind::kAlwaysBackedge},
            .child_bodies = {}});
    while_body_state.AddRootStmt(await_id);
  }

  BodyLoweringState process_body_state;
  const mir::ExprId cond_id =
      MakeTrueConditionExpr(process_body_state, unit_state.Builtins().bit1);
  std::vector<mir::Body> child_bodies;
  const mir::BodyId while_body_id =
      AddChildBody(child_bodies, while_body_state.Finish());
  const mir::StmtId while_stmt_id = process_body_state.AddStmt(
      mir::Stmt{
          .label = std::nullopt,
          .data = mir::WhileStmt{.condition = cond_id, .body = while_body_id},
          .child_bodies = std::move(child_bodies)});
  process_body_state.AddRootStmt(while_stmt_id);
  return mir::Process{
      .kind = mir::ProcessKind::kAlways, .body = process_body_state.Finish()};
}

}  // namespace

auto LowerProcess(
    const UnitLoweringState& unit_state, const ClassLoweringState& class_state,
    const hir::Process& src, TimeResolution time_resolution)
    -> diag::Result<mir::Process> {
  auto kind_or = LowerProcessKind(src.kind, src.span);
  if (!kind_or) return std::unexpected(std::move(kind_or.error()));

  ProcessLoweringState proc_state{time_resolution};
  if (*kind_or == mir::ProcessKind::kInitial) {
    return LowerInitialProcess(unit_state, class_state, src, proc_state);
  }
  return LowerAlwaysProcess(unit_state, class_state, src, proc_state);
}

}  // namespace lyra::lowering::hir_to_mir
