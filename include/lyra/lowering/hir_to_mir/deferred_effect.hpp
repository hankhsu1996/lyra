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
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

// The effect is due in a region this statement can name: this slot's, or the
// one `delay` names. The region is handed the closure and the procedure carries
// on (LRM 4.4.2.4, 9.4.5).
auto SubmitToDueRegion(
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

// Takes the carrier on as an execution of no lineage. LRM 9.4.5 makes no
// process of an update event, so nothing that names processes may find it (LRM
// 9.6.1, 9.6.3).
auto RunCarrierDetached(
    ProcessLowerer& process, WalkFrame frame, mir::Expr carrier) -> mir::Expr;

// The two halves a deferred effect is written as, which the standard's own
// division settles: `capture` reads everything the effect needs into the
// closure it is handed, where the statement is reached, and answers with it;
// `apply` then states the effect against what was captured, by appending to the
// body it is handed. Appending is what stating an effect is, because one
// statement's effect may be several writes -- an `apply` answering with a value
// would have it built and dropped, leaving a body that does nothing.
template <typename Capture, typename Apply>
concept DeferredEffectPlan = requires(
    Capture capture, Apply apply, ClosureBuilder& closure, mir::Block& body) {
  { apply(body, *capture(closure)) } -> std::same_as<void>;
};

// An effect the source wrote as one statement and the standard makes due later
// (LRM 4.4.2.4): a nonblocking assignment's update, a nonblocking event
// trigger. Everything the effect needs is read where the statement is reached
// and only the applying waits, and one statement's whole effect goes in one of
// these, so a control it carries is read once however many places it writes.
template <typename Capture, typename Apply>
  requires DeferredEffectPlan<Capture, Apply>
auto BuildDeferredEffect(
    ProcessLowerer& process, WalkFrame frame,
    const std::optional<hir::DelayOrEventControl>& control, Capture capture,
    Apply apply) -> diag::Result<mir::Expr> {
  ClosureBuilder closure(process.Owner().Unit(), frame);
  auto captured = capture(closure);
  if (!captured) return std::unexpected(std::move(captured.error()));

  // An event control is the one form whose slot is not knowable here, so it is
  // the one that needs an execution of its own to find it; every other form
  // hands the region a closure and is done.
  const auto* delay =
      control.has_value() ? std::get_if<hir::DelayControl>(&*control) : nullptr;
  const bool finds_its_own_slot = control.has_value() && delay == nullptr;
  if (finds_its_own_slot) {
    auto arrival = AppendArrivalAtDueRegion(process, frame, closure, *control);
    if (!arrival) return std::unexpected(std::move(arrival.error()));
  }

  // The steps the update runs are the closure's own, so they append to its
  // body: a name the enclosing body declared is not one this body can reach.
  apply(closure.Body(), *captured);

  if (finds_its_own_slot) {
    return RunCarrierDetached(process, frame, closure.BuildCoroutine());
  }
  return SubmitToDueRegion(
      process, frame,
      delay != nullptr ? std::optional<hir::DelayControl>{*delay}
                       : std::nullopt,
      closure.BuildVoid());
}

}  // namespace lyra::lowering::hir_to_mir
