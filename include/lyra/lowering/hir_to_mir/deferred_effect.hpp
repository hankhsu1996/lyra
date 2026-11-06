#pragma once

#include <concepts>
#include <expected>
#include <optional>
#include <utility>
#include <variant>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/timing.hpp"
#include "lyra/lowering/hir_to_mir/closure_builder.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/support/callable_bounds.hpp"

namespace lyra::lowering::hir_to_mir {

// The effect is due in a region this statement can name: this slot's, or the
// one `delay` names. The closure is made where that region's effects say and
// kept by them, and the procedure carries on (LRM 4.4.2.4, 9.4.5).
auto KeepInDueRegion(
    ProcessLowerer& process, WalkFrame frame,
    const std::optional<hir::DelayControl>& delay, mir::Expr closure)
    -> diag::Result<mir::Expr>;

// The effect is due in a slot nothing here can compute, so what is handed over
// is an execution that finds it. Appends to `carrier`'s body the waiting for
// the event and the arrival in the region the effect is due in; the caller
// appends the effect itself after them.
auto AppendArrivalAtDueRegion(
    ProcessLowerer& process, WalkFrame outer_frame, ClosureBuilder& carrier,
    const hir::DelayOrEventControl& control) -> diag::Result<void>;

// The instance a synchronous drive reaches through a virtual interface, where
// its landing is made on the one its target is reached on (LRM 14.16, 25.9):
// the handle the target's access holds, and that instance evaluated once in
// `frame`'s block, for both to be made on. Nothing for every other assignment.
struct DriveInstance {
  hir::ExprId handle;
  mir::ExprId object;
};
[[nodiscard]] auto DriveInstanceOf(
    ProcessLowerer& process, const WalkFrame& frame,
    const hir::AssignExpr& assign)
    -> diag::Result<std::optional<DriveInstance>>;

// How an execution carrying an effect whose slot `control` finds is run, and
// whether it goes on to its thread's NBA region once the slot is named. Either
// way it is of no lineage, since the standard makes no process of the effect
// and nothing that names processes may find it (LRM 9.6.1, 9.6.3). A
// nonblocking update's carrier runs apart and lands in its thread's NBA region
// (LRM 4.4.2.4, 9.4.5). A synchronous drive's runs inside each occurrence it
// waits for, where the runtime runs what an occurrence woke in the order it
// began to wait, so drives of one block maturing together land in the order
// they were made, and its landing has already returned in the Re-NBA region
// it lands in (LRM 14.16, 14.16.2).
struct Carrier {
  support::BuiltinFn run;
  bool arrives_in_nba_region;
};
[[nodiscard]] auto CarrierOf(const hir::DelayOrEventControl& control)
    -> Carrier;

// Takes `started`, the coroutine a carrier's body answers once called, on as an
// execution of no lineage, run by the entry `run`.
auto RunCarrier(
    ProcessLowerer& process, WalkFrame frame, support::BuiltinFn run,
    mir::Expr started) -> mir::Expr;

// The two halves a deferred effect is written as, which the standard's own
// division settles: `capture` reads everything the effect needs into the
// closure it is handed, where the statement is reached, and answers with it;
// `apply` then states the effect against what was captured, by appending to the
// body it is handed. Appending is what stating an effect is, because one
// statement's effect may be several writes -- an `apply` answering with a value
// would have it built and dropped, leaving a body that does nothing.
template <typename Capture, typename Apply>
concept DeferredEffectPlan = requires(
    Capture capture, Apply apply, ClosureBuilder& closure, mir::Builder body) {
  { apply(body, *capture(closure)) } -> std::same_as<void>;
};

// An effect the source wrote as one statement and the standard makes due later
// (LRM 4.4.2.4): a nonblocking assignment's update, a nonblocking event
// trigger. Everything the effect needs is read where the statement is reached
// and only the applying waits, and one statement's whole effect goes in one of
// these, so a control it carries is read once however many places it writes.
// Where the body runs as a function call, the statement first asks whether the
// thread calling it may schedule past the call's return (LRM 13.4.4).
template <typename Capture, typename Apply>
  requires DeferredEffectPlan<Capture, Apply>
auto BuildDeferredEffect(
    ProcessLowerer& process, WalkFrame frame,
    const std::optional<hir::DelayOrEventControl>& control, Capture capture,
    Apply apply) -> diag::Result<mir::Expr> {
  AppendScheduleAfterReturnRequirement(process.Owner(), frame);
  // An event control and a synchronous drive's control are the forms whose
  // slot is not knowable here, so they need an execution of their own to find
  // it, which takes the closure over; a delay, or no control, makes the closure
  // where the region's effects say and is done.
  const auto* delay =
      control.has_value() ? std::get_if<hir::DelayControl>(&*control) : nullptr;
  if (control.has_value() && delay == nullptr) {
    ClosureBuilder carrier(
        process.Owner().Unit(), frame, support::ClosureKind::kFnOnce);
    auto captured = capture(carrier);
    if (!captured) return std::unexpected(std::move(captured.error()));
    auto arrival = AppendArrivalAtDueRegion(process, frame, carrier, *control);
    if (!arrival) return std::unexpected(std::move(arrival.error()));
    // The steps the update runs are the closure's own, so they append to its
    // body: a name the enclosing body declared is not one this body can reach.
    apply(carrier.Body(), *captured);
    return RunCarrier(
        process, frame, CarrierOf(*control).run, carrier.BuildCallOnce());
  }

  ClosureBuilder closure(
      process.Owner().Unit(), frame, support::ClosureKind::kFn);
  auto captured = capture(closure);
  if (!captured) return std::unexpected(std::move(captured.error()));
  apply(closure.Body(), *captured);
  return KeepInDueRegion(
      process, frame,
      delay != nullptr ? std::optional<hir::DelayControl>{*delay}
                       : std::nullopt,
      closure.BuildVoid());
}

}  // namespace lyra::lowering::hir_to_mir
