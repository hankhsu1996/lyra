#include "lyra/lowering/hir_to_mir/deferred_effect.hpp"

#include <cstdint>
#include <expected>
#include <optional>
#include <utility>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/timing.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/closure_builder.hpp"
#include "lyra/lowering/hir_to_mir/integral_literal.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/lowering/hir_to_mir/statement/loops.hpp"
#include "lyra/lowering/hir_to_mir/statement/timing.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// The commit of `closure_id` into this slot's region, which is where an effect
// carrying no control is due (LRM 4.4.2.4).
auto BuildNbaSubmitCall(
    const mir::CompilationUnit& unit, mir::ExprId runtime_id,
    mir::ExprId closure_id) -> mir::Expr {
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee =
                  mir::Direct{
                      .target = support::BuiltinFn::kSubmitNba,
                      .receiver = runtime_id},
              .arguments = {closure_id}},
      .type = unit.builtins.void_type};
}

// The same commit into the region of the slot a delay names (LRM 9.4.5). The
// amount crosses unscaled with its scope's powers, because LRM 9.4.1 reads a
// delay expression's own value before any scaling.
//
// Which entry carries it follows the amount's own type, because the language
// reads the same written value differently in each: an integral amount counts
// whole time units, while a real one may name a fraction of one.
auto BuildNbaSubmitAfterCall(
    ProcessLowerer& process, WalkFrame frame, const hir::DelayControl& delay,
    mir::ExprId runtime_id, mir::ExprId closure_id) -> diag::Result<mir::Expr> {
  auto& unit = process.Owner().Unit();
  auto& block = *frame.current_block;
  auto duration_or =
      process.LowerExpr(process.HirBody().exprs.Get(delay.duration), frame);
  if (!duration_or) return std::unexpected(std::move(duration_or.error()));
  mir::ExprId duration_id = block.exprs.Add(*std::move(duration_or));
  const mir::Type& duration_type =
      unit.types.Get(block.exprs.Get(duration_id).type);
  const bool is_real = duration_type.IsRealFamily();
  if (duration_type.Is<mir::ShortRealType>()) {
    // LRM 6.12.1: `real` and `realtime` are one type, and a `shortreal` differs
    // from them only in host precision, so the entry takes the wider.
    duration_id =
        ConvertToType(unit, block, duration_id, unit.builtins.realtime);
  }
  const mir::ExprId unit_power_id = BuildIntLiteral(
      unit, block, static_cast<std::int64_t>(process.Resolution().unit_power));
  const mir::ExprId precision_power_id = BuildIntLiteral(
      unit, block,
      static_cast<std::int64_t>(process.Resolution().precision_power));
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee =
                  mir::Direct{
                      .target = is_real
                                    ? support::BuiltinFn::kSubmitNbaAfterReal
                                    : support::BuiltinFn::kSubmitNbaAfter,
                      .receiver = runtime_id},
              .arguments =
                  {duration_id, unit_power_id, precision_power_id, closure_id}},
      .type = unit.builtins.void_type};
}

// The wait an event control is, built into the carrier's body. A repeat count
// is how many occurrences of that event the effect waits out; LRM 9.4.5 reads
// it once where the statement is reached, so it is lowered in the outer frame
// and carried in, and a count of none reaches the effect straight away.
auto BuildDueSlotWaitStmt(
    ProcessLowerer& process, WalkFrame outer_frame, ClosureBuilder& carrier,
    const hir::DelayOrEventControl& control) -> diag::Result<mir::Stmt> {
  UnitLowerer& unit_lowerer = process.Owner();
  const WalkFrame carrier_frame = carrier.Frame();
  mir::Block& body = carrier.Body();
  return std::visit(
      Overloaded{
          [&](const hir::EventControl& ec) -> diag::Result<mir::Stmt> {
            return BuildEventWaitStmt(
                process, process.EnclosingScopeLowerer(), carrier_frame, body,
                ec);
          },
          [&](const hir::NamedEventControl& nec) -> diag::Result<mir::Stmt> {
            return BuildNamedEventWaitStmt(process, carrier_frame, body, nec);
          },
          [&](const hir::RepeatedEventControl& r) -> diag::Result<mir::Stmt> {
            auto count_or = process.LowerExpr(
                process.HirBody().exprs.Get(r.count), outer_frame);
            if (!count_or) return std::unexpected(std::move(count_or.error()));
            const mir::ExprId count = SnapshotIntoClosure(
                unit_lowerer, outer_frame, carrier,
                outer_frame.current_block->exprs.Add(*std::move(count_or)),
                "_lyra_nba_count");
            mir::Block loop_body;
            const WalkFrame loop_frame = carrier_frame.WithBlock(&loop_body);
            auto wait_or =
                BuildAnyEventWaitStmt(process, loop_frame, loop_body, r.event);
            if (!wait_or) return std::unexpected(std::move(wait_or.error()));
            loop_body.AppendStmt(*std::move(wait_or));
            return BuildRepeatLoopStmt(
                unit_lowerer.Unit(), carrier_frame, body, count,
                body.child_scopes.Add(std::move(loop_body)));
          },
          [](const hir::DelayControl&) -> diag::Result<mir::Stmt> {
            throw InternalError(
                "BuildDueSlotWaitStmt: a delay names the slot outright, so "
                "nothing has to wait to find out which one it is");
          }},
      control);
}

}  // namespace

auto SubmitToDueRegion(
    ProcessLowerer& process, WalkFrame frame,
    const std::optional<hir::DelayControl>& delay, mir::Expr closure)
    -> diag::Result<mir::Expr> {
  auto& block = *frame.current_block;
  const mir::ExprId closure_id = block.exprs.Add(std::move(closure));
  const mir::ExprId runtime_id =
      block.exprs.Add(BuildCurrentRuntimeCallExpr(process.Owner()));
  if (!delay.has_value()) {
    return BuildNbaSubmitCall(process.Owner().Unit(), runtime_id, closure_id);
  }
  return BuildNbaSubmitAfterCall(
      process, frame, *delay, runtime_id, closure_id);
}

auto AppendArrivalAtDueRegion(
    ProcessLowerer& process, WalkFrame outer_frame, ClosureBuilder& carrier,
    const hir::DelayOrEventControl& control) -> diag::Result<void> {
  UnitLowerer& unit_lowerer = process.Owner();
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Block& body = carrier.Body();

  auto wait_or = BuildDueSlotWaitStmt(process, outer_frame, carrier, control);
  if (!wait_or) return std::unexpected(std::move(wait_or.error()));
  body.AppendStmt(*std::move(wait_or));

  // The event has named the slot; the region within it is the one every
  // nonblocking effect lands in, so the carrier goes there before applying it
  // (LRM 4.4.2.4).
  const mir::ExprId runtime_id =
      body.exprs.Add(BuildCurrentRuntimeCallExpr(unit_lowerer));
  const mir::ExprId region_call_id = body.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = support::BuiltinFn::kResumeInNbaRegion},
                  .arguments = {runtime_id}},
          .type = unit.builtins.void_type});
  body.AppendStmt(
      mir::ExprStmt{
          .expr = body.exprs.Add(
              mir::Expr{
                  .data = mir::AwaitExpr{.awaitable = region_call_id},
                  .type = unit.builtins.void_type})});
  return {};
}

auto RunCarrierDetached(
    ProcessLowerer& process, WalkFrame frame, mir::Expr carrier) -> mir::Expr {
  auto& block = *frame.current_block;
  const mir::ExprId carrier_id = block.exprs.Add(std::move(carrier));
  const mir::ExprId runtime_id =
      block.exprs.Add(BuildCurrentRuntimeCallExpr(process.Owner()));
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee =
                  mir::Direct{
                      .target = support::BuiltinFn::kRunDetached,
                      .receiver = runtime_id},
              .arguments = {carrier_id}},
      .type = process.Owner().Unit().builtins.void_type};
}

}  // namespace lyra::lowering::hir_to_mir
