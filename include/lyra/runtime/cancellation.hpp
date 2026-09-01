#pragma once

#include <cstdint>
#include <exception>

#include "lyra/runtime/registration.hpp"

namespace lyra::runtime {

class RuntimeEffects;
class RuntimeProcess;

// The per-instance cancellation state of a procedural scope (LRM 9.6.2
// `disable`). It is a reusable cancellation token: one monotonic
// generation plus the set of activations currently blocked inside the target.
// An execution entering the target captures the current generation; where it
// regains control it compares its captured generation against the current one,
// and a mismatch means it was disabled while away. `disable` advances the
// generation and wakes every blocked activation so each reaches that
// comparison. The generation is a counter rather than a one-shot flag because
// the same target is re-entered under one static identity (a reentrant task, an
// `always`); an entry after a disable captures the newer generation and is
// unaffected, which a boolean could not express.
//
// This is the cancellation token of async runtimes, made reusable. It is per
// instance of the enclosing structural scope -- shared by every concurrent
// execution inside it -- so it outlives any single execution and is stored as a
// member of that instance. It is not an execution scope: it owns no
// activation's lifetime, and the waiters it holds are revocable registrations
// the activations themselves own, exactly as an event's are. Where a runtime
// with a separate token factory would have two objects, this is one, and it is
// named for what a `disable` names -- the word the LRM uses.
//
// Two things a reader arriving from those runtimes will look for and not find,
// both deliberate. A cancellation reaches an execution by polling and not by a
// callback: it is reconciled where the execution regains control, because a
// simulated process cannot be made to run arbitrary code partway through a
// statement of the design. And there is one strength of cancellation rather
// than three -- `disable` ends its target's extent outright, and the language
// offers no partial form there would be anything to honour.
class CancellationTarget {
 public:
  [[nodiscard]] auto Generation() const -> std::uint64_t {
    return generation_;
  }

  // A target is something an execution can be waiting on: while blocked inside
  // it, the execution waits for the target not to be disabled, alongside
  // whatever else it waits for. It enrolls here for the same reason and by the
  // same means it enrolls in an event.
  [[nodiscard]] auto CancelWaiters() -> RegistrationList& {
    return cancel_waiters_;
  }

  // LRM 9.6.2 `disable`: end the current generation, then wake every execution
  // blocked inside the target so each reconciles against the new one where it
  // regains control.
  void Invalidate(RuntimeEffects& effects);

 private:
  std::uint64_t generation_ = 0;
  RegistrationList cancel_waiters_;
};

// The control effect that leaves an execution. It is transport, not a fault:
// the execution it leaves is not in error. It travels outward until a region
// naming its target consumes it and continues past that target; a called task
// merely passes it on. An effect no region claims runs out the whole body, and
// the activation settles as cancelled -- reported as KILLED (LRM 9.7).
//
// `target` says which region may claim it. A `disable` names the target it
// disabled (LRM 9.6.2). A `kill`, and a `disable` of the execution itself, name
// nothing: no region can match `nullptr`, so the effect is unclaimable by
// construction and the activation is the only thing it can end. One type covers
// both because they differ only in whether anyone is allowed to catch them.
//
// It deliberately does not derive from `std::exception`: an effect that escapes
// its owner is a compiler defect, and staying outside that hierarchy keeps it
// from being absorbed by a boundary meant for user-facing errors.
struct ControlEffect {
  CancellationTarget* target;
};

// Raises the unclaimable form: the execution ends here and now, with no region
// able to intercept it (`kill`, LRM 9.7; a self-`disable`, LRM 9.6.2). Call
// only after the termination has been recorded.
[[noreturn]] void RaiseUnclaimableEffect();

// Raises the effect a body reported as its outcome instead of by leaving, so
// the activation that drove that body settles cancelled (LRM 9.6.2). Call from
// the driver once the body has run its last statement.
[[noreturn]] void RaiseControlEffect(CancellationTarget* target);

// What came out of a body that did not return: a control effect, or a fault. A
// body left by unwinding delivers the two identically, so one shape carries
// either.
struct Unwound {
  // False for a fault. A frame that is not the effect's landing re-raises it
  // either way; only a landing needs to tell them apart.
  bool control_effect;
  std::exception_ptr raised;
};

// Answers which of the two just unwound. It has to be asked rather than looked
// at: while an exception is being handled its type is knowable only by raising
// it again into a handler, and once it is put away as an `exception_ptr` even
// that is gone. Asking here keeps the effect's type named in the one place that
// raises it, so nowhere else has to name it to know what it was looking at.
//
// Call only while an exception is being handled -- for an activation, the one
// place it settles without having returned.
[[nodiscard]] auto ClassifyUnwind() -> Unwound;

// The two ends of an execution's membership of a target (LRM 9.6.2). Entering
// captures the target's current generation and marks the executing process as
// inside it, so a `disable` of the target reaches this execution and a check
// finds the target among the ones it is inside; leaving withdraws that. The
// two bracket the target's extent, and the extent's ways out are the caller's
// to enumerate.
void EnterCancellationTarget(
    RuntimeEffects& effects, CancellationTarget* target);
void LeaveCancellationTarget(
    RuntimeEffects& effects, CancellationTarget* target);

// LRM 9.6.2: raises the pending effect, if any, for an execution regaining
// control. Reached where an execution can next run a user statement -- a leaf
// wait resuming, and the `disable` statement itself -- so no execution runs a
// statement of a target that was disabled while it was inside it. The outermost
// invalidated target wins, because leaving it also leaves everything nested in
// it.
void RaiseControlEffectIfDisabled(RuntimeProcess& process);

// Whether a landing may claim the effect it is holding (LRM 9.6.2): true when
// the effect names the target this landing owns, so execution resumes past that
// target. A pure test; the landing acts on the answer itself.
auto EffectNamesTarget(const ControlEffect& effect, CancellationTarget* target)
    -> bool;

// LRM 9.6.2 `disable`: invalidate the target, wake the executions blocked
// inside it, then raise the effect in the disabling execution itself when it
// is inside the target too -- which is what makes a self-disable leave
// immediately rather than at some later boundary.
void Disable(CancellationTarget* target, RuntimeEffects& effects);

}  // namespace lyra::runtime
