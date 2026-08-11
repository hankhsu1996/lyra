#pragma once

#include <cstdint>

#include "lyra/runtime/registration.hpp"

namespace lyra::runtime {

class RuntimeEffects;
class RuntimeProcess;

// The per-instance cancellation state of a procedural scope (LRM 9.6.2
// `disable`). It is a reusable cancellation source: one monotonic
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
// This is the cancellation-token source of async runtimes, made reusable. It is
// per instance of the enclosing structural scope -- shared by every concurrent
// execution of the target -- so it outlives any single execution and is stored
// as a member of that instance. It is not an execution scope: it owns no
// activation's lifetime, and the waiters it holds are revocable registrations
// the activations themselves own, exactly as an event's are.
class CancellationSource {
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

// The control effect a `disable` raises in an execution that is inside the
// disabled target (LRM 9.6.2). It is transport, not a fault: the execution it
// leaves is not in error, and where it lands depends on who owns the target --
// the owning frame consumes it and continues past the target, a called task
// merely unwinds through, and a spawned branch that owns no landing ends as
// KILLED (LRM 9.7).
//
// It deliberately does not derive from `std::exception`: an abort that escapes
// its owner is a compiler defect, and staying outside that hierarchy keeps it
// from being absorbed by a boundary meant for user-facing errors.
struct Abort {
  CancellationSource* target;
  // One effect unwinds one execution's frames, so it names the execution whose
  // outcome it decides.
  RuntimeProcess* leaving;
};

// Holds an execution's membership of a target for the target's extent (LRM
// 9.6.2). It captures the generation on entry and marks the executing process
// as inside the target, so a `disable` of the target reaches this execution and
// a check finds the target among the ones it is inside. Pushed on entry and
// popped on every exit -- normal fall-through or an abort unwinding through --
// by tying push/pop to a stack object's lifetime.
class CancellationGuard {
 public:
  CancellationGuard(RuntimeEffects& effects, CancellationSource* source);
  CancellationGuard(const CancellationGuard&) = delete;
  auto operator=(const CancellationGuard&) -> CancellationGuard& = delete;
  CancellationGuard(CancellationGuard&&) = delete;
  auto operator=(CancellationGuard&&) -> CancellationGuard& = delete;
  ~CancellationGuard();

 private:
  RuntimeProcess* process_;
  CancellationSource* source_;
};

// LRM 9.6.2: raises the pending abort, if any, for an execution regaining
// control. Reached where an execution can next run a user statement -- a leaf
// wait resuming, and the `disable` statement itself -- so no execution runs a
// statement of a target that was disabled while it was inside it. The outermost
// invalidated target wins, because leaving it also leaves everything nested in
// it.
void RaiseAbortIfDisabled(RuntimeProcess& process);

// The landing of a target's owning frame (LRM 9.6.2): consumes `abort` when it
// names `target`, which resumes execution past the target, and re-raises it
// otherwise so it reaches the frame that does own it.
void AbortConsumeOrRethrow(const Abort& abort, CancellationSource& target);

// LRM 9.6.2 `disable`: invalidate the target, wake the executions blocked
// inside it, then raise the abort in the disabling execution itself when it is
// inside the target too -- which is what makes a self-disable leave immediately
// rather than at some later boundary.
void Disable(CancellationSource& target, RuntimeEffects& effects);

}  // namespace lyra::runtime
