#include "lyra/lowering/hir_to_mir/expression/system/diagnostic.hpp"

#include <cstddef>
#include <cstdint>
#include <expected>
#include <utility>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_manager.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/hir_to_mir/expression/system/control.hpp"
#include "lyra/lowering/hir_to_mir/print_items.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// The report a severity task makes: its message formatted from the items at
// `items_offset` on, with decimal as the bare-argument radix (LRM 20.10,
// 21.2.1.4), handed to the diagnostic dispatcher at the severity `entry`
// names. The call's source location is the origin the dispatcher prints and
// keeps each site's reports apart by.
auto BuildReportCall(
    ProcessLowerer& process, const WalkFrame& frame, const hir::CallExpr& call,
    support::BuiltinFn entry, std::size_t items_offset, diag::SourceSpan span)
    -> diag::Result<mir::Expr> {
  auto items_or = BuildDisplayListPrintItems(
      process, frame, call, support::PrintRadix::kDecimal, items_offset);
  if (!items_or) return std::unexpected(std::move(items_or.error()));
  auto& unit = process.Owner().Unit();
  auto& block = *frame.current_block;
  const auto time_unit_power =
      static_cast<std::int64_t>(process.Resolution().unit_power);
  const mir::ExprId items_array = block.exprs.Add(
      BuildPrintItemsArray(unit, block, *items_or, time_unit_power));
  const mir::ExprId text_id =
      block.exprs.Add(BuildFormatCallExpr(unit, block, items_array));
  const mir::ExprId diagnostic_id =
      block.exprs.Add(BuildDiagnosticCallExpr(unit, block));
  const mir::ExprId origin_id = BuildStringValueExpr(
      unit, block,
      diag::RuntimeOriginOf(span, process.Owner().SourceManager()));
  return BuildReportCallExpr(unit, entry, diagnostic_id, origin_id, text_id);
}

}  // namespace

auto LowerDiagnosticSystemSubroutineCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call,
    const support::DiagnosticSystemSubroutineInfo& info, diag::SourceSpan span)
    -> diag::Result<mir::Expr> {
  if (info.builtin_fn != support::BuiltinFn::kEmitFatal) {
    return BuildReportCall(process, frame, call, info.builtin_fn, 0, span);
  }

  // $fatal takes its first argument as the finish number (LRM 20.10), held in
  // a local before the message's arguments are evaluated, and ends the run
  // with an implicit $finish after the report. The report is a statement of
  // its own, and the $finish is this lowering's expression, which departs from
  // the calling execution rather than returning.
  auto finish_number = BuildFinishNumber(process, frame, call);
  if (!finish_number) return std::unexpected(std::move(finish_number.error()));
  const mir::ExprId held = EvaluatedOnce(frame, *finish_number);
  auto reported = BuildReportCall(
      process, frame, call, info.builtin_fn, call.arguments.empty() ? 0 : 1,
      span);
  if (!reported) return std::unexpected(std::move(reported.error()));
  auto& block = *frame.current_block;
  block.AppendStmt(
      mir::ExprStmt{.expr = block.exprs.Add(*std::move(reported))});
  return BuildEndRunCall(
      process, block, support::BuiltinFn::kFinish, held, span);
}

}  // namespace lyra::lowering::hir_to_mir
