#pragma once

#include <cstdint>
#include <exception>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/intrusive_list.hpp"

namespace lyra::runtime {

class RuntimeEffects;
class RuntimeProcess;

// How an execution came to be inside a target, which decides what a `disable`
// of it does to the execution (LRM 9.6.2).
enum class MembershipOrigin : std::uint8_t {
  // The execution entered the target, and a `disable` resumes it at the
  // statement after it.
  kEntered,
  // The execution was started inside the target, an activity enabled within
  // it, so nothing of it lies past the target and a `disable` of it ends the
  // execution.
  kEnabledWithin,
};

// A process's being inside a disable target (LRM 9.6.2), as a node of the
// target's list. The process's record of the target owns it, so it stands for
// exactly as long as the process is inside.
struct TargetMembership : IntrusiveListNode<TargetMembership> {
  // Null on the sentinel a list embeds to close its ring.
  RuntimeProcess* process = nullptr;
  MembershipOrigin origin = MembershipOrigin::kEntered;
};

// The cancellation state of a procedural scope (LRM 9.6.2
// `disable`). It is a reusable cancellation token: one monotonic
// generation plus the set of processes currently inside the target.
// An execution entering the target captures the current generation; where it
// regains control it compares its captured generation against the current one,
// and a mismatch means it was disabled while away. `disable` advances the
// generation and wakes every one of them that is blocked so each reaches that
// comparison -- except an activity enabled within the target, which has nothing
// past the target to reach and is ended where it stands. The generation is a
// counter rather than a one-shot flag because the same target is re-entered
// under one static identity (a reentrant task, an `always`); an entry after a
// disable captures the newer generation and is unaffected, which a boolean
// could not express.
//
// This is the cancellation token of async runtimes, made reusable. There is one
// per thing that replicates the scope -- a module instance, and nothing at all
// for a class method or a package subroutine, where one serves the whole class
// or the whole program -- and it is shared by every concurrent execution inside
// the target, so it outlives any single execution and is stored as a cell of
// whatever owns it. It is not an execution scope: it owns no
// activation's lifetime, and the memberships it holds are owned by the
// processes inside it, exactly as an event's are owned by what waits on it.
// Where a runtime with a separate token factory would have two objects, this is
// one, and it is named for what a `disable` names -- the word the LRM uses.
//
// Two things a reader arriving from those runtimes will look for and not find,
// both deliberate. A cancellation reaches an execution by polling and not by a
// callback: it is reconciled where the execution regains control, because a
// simulated process cannot be made to run arbitrary code partway through a
// statement of the design; an execution the cancellation ends instead runs
// nothing of the design on its way out, so it is ended at once, as `kill` ends
// one. And there is one strength of cancellation rather
// than three -- `disable` ends its target's extent outright, and the language
// offers no partial form there would be anything to honour.
class CancellationTarget {
 public:
  // Defined in the library: whatever replicates a scope constructs and destroys
  // one, and a definition written here would be compiled again by every unit
  // that does.
  CancellationTarget();
  CancellationTarget(const CancellationTarget&) = delete;
  auto operator=(const CancellationTarget&) -> CancellationTarget& = delete;
  CancellationTarget(CancellationTarget&&) = delete;
  auto operator=(CancellationTarget&&) -> CancellationTarget& = delete;
  ~CancellationTarget();

  [[nodiscard]] auto Generation() const -> std::uint64_t {
    return generation_;
  }

  // A target is something an execution can be waiting on: while blocked inside
  // it, the execution waits for the target not to be disabled, alongside
  // whatever else it waits for. A process enrols here for as long as it is
  // inside, as a wait enrols on what it watches for as long as it stands.
  [[nodiscard]] auto Members() -> IntrusiveList<TargetMembership>& {
    return members_;
  }

  // LRM 9.6.2 `disable`: end the current generation, wake every execution
  // blocked inside the target that entered it, so each reconciles against the
  // new generation where it regains control and resumes past it, and end every
  // activity enabled within it, as `kill` ends one (LRM 9.7).
  void Invalidate(RuntimeEffects& effects);

 private:
  std::uint64_t generation_ = 0;
  IntrusiveList<TargetMembership> members_;
};

// A disable target together with the generation it carried when something
// recorded it (LRM 9.6.2). Whatever recorded it holds only while the two still
// agree: a `disable` advances the generation, and every record taken before it
// stops holding at once, without the target keeping any set of them. Recording
// it is what makes the relation answerable by a frame that does not know which
// target it is holding, and by an effect that outlives the statement that
// created it.
struct CapturedTarget {
  CancellationTarget* target;
  std::uint64_t generation;

  [[nodiscard]] auto Holds() const -> bool {
    return target->Generation() == generation;
  }
};

// The control effect that leaves an execution. It is transport, not a report:
// the execution it leaves is not in error. It travels outward until a region
// naming its target consumes it and continues past that target; a called task
// merely passes it on. An effect no region can claim runs out the whole body,
// and the activation settles as cancelled -- reported as KILLED (LRM 9.7).
//
// `target` says which region may claim it. A `disable` names the target it
// disabled (LRM 9.6.2). A `kill`, a `disable` of a target the execution was
// started within, a task that ends the run, and a run-time error a landing has
// received name nothing: no region can match `nullptr`, so the effect is
// unclaimable by construction and the activation is the only thing it can end.
// One type covers every form because they differ only in whether anyone is
// allowed to catch them.
//
// It deliberately does not derive from `std::exception`: an effect that escapes
// its owner is a compiler defect, and staying outside that hierarchy keeps it
// from being absorbed by a boundary meant for user-facing errors.
struct ControlEffect {
  CancellationTarget* target;
};

// Raises the unclaimable form: the execution ends here and now, with no region
// able to intercept it (`kill`, LRM 9.7; a `disable` of a target the execution
// was started within, LRM 9.6.2; a task that ends the run, LRM 20.2). Call only
// after the termination has been recorded.
[[noreturn]] void RaiseUnclaimableEffect();

// Raises again the departure a handler received and declines, so a handler
// further out tests the target it names (LRM 9.6.2). What is raised is the
// departure as received, which for a run-time error is the one it became.
[[noreturn]] void RaiseDeclinedDeparture(ControlEffect effect);

// What came out of a body that did not return: a control effect, or a run-time
// error. A body left by unwinding delivers the two identically, so one shape
// carries either.
//
// Its special members are the library's: a unit whose frames settle an outcome
// copies and destroys one, and a definition written here would be compiled
// again by each such unit.
struct Unwound {
  Unwound(bool control_effect, std::exception_ptr raised);
  Unwound(const Unwound&);
  auto operator=(const Unwound&) -> Unwound&;
  Unwound(Unwound&&) noexcept;
  auto operator=(Unwound&&) noexcept -> Unwound&;
  ~Unwound();

  // False for a run-time error. A frame that is not the landing carries either
  // outward unchanged; only a landing acts on which it was.
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

// Makes a raised error the ending it is (LRM 20.10): reports it, ends the run,
// and asks the running execution, if there is one, to stop. A design's error
// is the fatal report and the run ends as `$fatal` ends it; a failure of the
// tool ends the run without the design's final procedures. What remains is the
// departure, which the caller states the way its own frame leaves.
void SettleRaised(RuntimeEffects& effects, const std::exception_ptr& raised);

// What a handler received, as the control effect it now carries. A handler
// receives whatever unwinds into it: a control effect is carried as it is, and
// a run-time error is settled here and carried on as the departure no region
// may claim, so it is reported once, by the first handler it reaches, and
// every cleanup it passes after that runs as for any departure. Anything else
// -- the release of a suspended stack, which passes every frame running what
// each owes -- is raised again from here, unchanged, so a handler names no
// raised type.
//
// Call only while an exception is being handled, as above.
[[nodiscard]] auto ReceiveDeparture() -> ControlEffect;

// Runs `stretch`, a stretch of the design's code that is settled where it ends
// rather than by an activation running it, as its own landing, which settles it
// the way an activation's landing settles a body: a departure no region claimed
// ends it there, and a run-time error that left it is settled, and either way
// the caller carries on. A claimable departure never arrives here: it names a
// region the departing execution is inside, and that region claims it first.
template <typename Stretch>
void RunAsLanding(RuntimeEffects& effects, Stretch&& stretch) {
  try {
    std::forward<Stretch>(stretch)();
  } catch (const ControlEffect& effect) {
    if (effect.target != nullptr) {
      throw InternalError(
          "RunAsLanding: a departure a region owns left every region");
    }
  } catch (const std::exception&) {
    SettleRaised(effects, std::current_exception());
  }
}

// The two ends of an execution's membership of a target (LRM 9.6.2). Entering
// captures the target's current generation and marks the executing process as
// inside it, so a `disable` of the target reaches this execution, which then
// resumes past it, and a check finds the target among the ones it is inside;
// leaving withdraws that. The two bracket the target's extent, and the
// extent's ways out are the caller's to enumerate.
void EnterCancellationTarget(
    RuntimeEffects& effects, CancellationTarget* target);
void LeaveCancellationTarget(
    RuntimeEffects& effects, CancellationTarget* target);

// LRM 9.6.2, 9.7: raises the departure owed here, if one is. Reached where an
// execution can next run a user statement -- a leaf wait resuming, a foreign
// call returning, and the `disable` statement itself -- so no execution runs a
// statement it has been told not to. The outermost invalidated target wins,
// because leaving it also leaves everything nested in it; a termination names
// no target, because nothing may continue past one.
void TakeDepartureIfDue(RuntimeProcess& process);

// The same question where the body itself states it, because nothing else runs
// at that point: a foreign call returns without having suspended anything when
// the foreign side consumes no simulation time, so this execution regains
// control with no resumption to ask on its behalf (LRM 35.9).
void TakeDepartureIfDue(RuntimeEffects& effects);

// Whether a region of `target` lands the effect it is holding (LRM 9.6.2),
// asked once the region has left its own extent: true when the effect names
// `target` and the execution then owes no further departure, so execution
// resumes past the region. A departure still owed -- the execution is inside
// the same target further out, as a reentrant task's earlier activation is, or
// its own termination is due -- means the region is not where the `disable`
// lands, and it raises the effect again for the one that is (LRM 9.6.2: every
// activation of a disabled task is disabled). The region acts on the answer
// itself.
auto RegionLandsEffect(
    RuntimeEffects& effects, const ControlEffect& effect,
    CancellationTarget* target) -> bool;

// LRM 9.6.2 `disable`: invalidate the target, which wakes or ends each
// execution inside it, then raise the effect in the disabling execution itself
// when it is inside the target too -- which is what makes a self-disable leave
// immediately rather than at some later boundary.
void Disable(CancellationTarget* target, RuntimeEffects& effects);

}  // namespace lyra::runtime
