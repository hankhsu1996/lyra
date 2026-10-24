#pragma once

#include <algorithm>
#include <cstdint>
#include <exception>
#include <list>
#include <memory>
#include <vector>

#include "lyra/runtime/cancellation.hpp"
#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/intrusive_list.hpp"
#include "lyra/runtime/owned_processes.hpp"
#include "lyra/runtime/rng.hpp"
#include "lyra/runtime/running_state.hpp"
#include "lyra/runtime/wait.hpp"
#include "lyra/support/target_landing.hpp"

namespace lyra::runtime {

class RuntimeEffects;
class ForeignExecution;
class Scope;

enum class ProcessExecutionState : std::uint8_t {
  kCreated,
  kRunning,
  kWaiting,
  kSuspended,
  kTerminated,
};

// Why a process reached its terminal state, a persistent fact of the process
// node rather than of its completion slot: a killed process is released while
// parked, so its slot is never read, yet `status()` (LRM 9.7) must still report
// KILLED through a surviving handle. Every terminal path settles one of these,
// and the execution state stays outcome-neutral (`kTerminated`).
enum class ProcessTerminationCause : std::uint8_t {
  // Ran to the end of its body. LRM 9.7 reports this as FINISHED.
  kCompleted,
  // Left by a departure rather than by reaching the end: `kill` (LRM 9.7), a
  // `disable` (LRM 9.6), or a run-time error the body had no way to carry on
  // from. Reported as KILLED.
  kKilled,
};

// One run -- one execution pass -- of a process's pending deferred reports: the
// deferred assertion reports of LRM 16.4.1 and the unique / priority violation
// reports of LRM 12.4.2.1, which share the one per-process queue. A flush point
// marks it withdrawn, which withdraws that pass's reports together without any
// of them being found. Termination is not a flush point, so a report queued
// before the body ended still matures, however soon the process itself goes.
struct DeferredReportEpoch {
  bool withdrawn = false;
};

// What a pending deferred report recorded where it was created, and the whole
// of what it asks before it acts: it acts only if every source it recorded
// still stands (LRM 16.4.1, 16.4.2, 16.4.4).
//
// Each source is held by the report itself, so asking never reaches the
// process that queued it, which may be gone by then: a disable target is
// per-instance storage that outlives every report holding it, and the pass is
// kept alive by the reports that recorded it.
struct DeferredReportValidity {
  std::shared_ptr<const DeferredReportEpoch> pass;
  std::vector<CapturedTarget> targets;

  [[nodiscard]] auto Holds() const -> bool {
    return !pass->withdrawn &&
           std::ranges::all_of(targets, [](const CapturedTarget& captured) {
             return captured.Holds();
           });
  }
};

// A node of the dynamic process lineage (LRM 9.5) and, while its body runs, the
// owner of the coroutine frame executing it. The two lifetimes are distinct:
// the frame is released the moment the body terminates, whereas the node
// survives for as long as any descendant is still alive. LRM 9.6.3 demands the
// separation -- `disable fork` terminates the descendants of subprocesses that
// have already terminated, so a terminated process stays reachable from its
// parent.
//
// A process's children are those it spawned itself, whether directly or from a
// task or function it called (LRM 9.6.1): a subroutine call runs in the
// caller's thread and creates no process of its own. They therefore accumulate
// across every fork the process executes.
class RuntimeProcess : public std::enable_shared_from_this<RuntimeProcess> {
 public:
  // `rng` is this process's own generator. LRM 18.14.1 fixes where it comes
  // from by how the process was created: a dynamically created one takes it
  // out of the process that created it, and one nothing that is a thread
  // creates -- a static process, an assertion's action -- out of the
  // initialization RNG of its context's instance.
  RuntimeProcess(
      ExecutionContext context, Coroutine<void> coroutine, DrawRng rng);

  RuntimeProcess(const RuntimeProcess&) = delete;
  auto operator=(const RuntimeProcess&) -> RuntimeProcess& = delete;
  // Non-movable: the coroutine promise stores a back-pointer to this
  // RuntimeProcess, and so does every child node, so its address must stay
  // stable for as long as either can name it. A node is retained by shared
  // ownership -- whoever started it while it or a descendant is live, and any
  // user-visible `process` handle (LRM 9.7) -- so its lifetime is reachability,
  // not a single owner.
  RuntimeProcess(RuntimeProcess&&) noexcept = delete;
  auto operator=(RuntimeProcess&&) noexcept -> RuntimeProcess& = delete;
  // Ends the foreign calls still suspended when the run ends, innermost first,
  // as nested extents end, because a call made inside another is part of it.
  // Out of line, because the vehicle that carries one is only named here.
  ~RuntimeProcess();

  // What is in force while this process runs: the generator its randomization
  // calls draw from and a process spawned from here takes its seed out of (LRM
  // 18.14.2), and the DPI scope chain its foreign calls report through (LRM
  // 35.5.3). It belongs to the process rather than to the run so that a
  // generator survives a suspension and two foreign calls suspended on
  // different processes never share a chain.
  [[nodiscard]] auto Running() -> RunningState& {
    return running_;
  }

  // What this process runs in, which an execution it starts takes as its own.
  // Read by a run stopped from outside, so it may not throw.
  [[nodiscard]] auto Context() const noexcept -> const ExecutionContext& {
    return context_;
  }

  // The process's internal execution state. A `process` handle (LRM 9.7) reads
  // it through `status()`, which projects it onto the LRM state enum; the state
  // survives the coroutine frame, so a terminated process is still observable.
  [[nodiscard]] auto ExecutionState() const -> ProcessExecutionState {
    return execution_state_;
  }

  // Why the process terminated -- meaningful only once `ExecutionState()` is
  // `kTerminated`. Distinguishes a FINISHED process from a KILLED one for
  // `status()` (LRM 9.7).
  [[nodiscard]] auto TerminationCause() const -> ProcessTerminationCause {
    return termination_cause_;
  }

  // Runs the process `activation` belongs to from `activation` -- the specific
  // frame that suspended (the innermost one when a task enabled by this
  // process is suspended) -- and releases the process and its lineage once it
  // has terminated. The process is held for the whole run, because its own
  // ending wakes what may drop every other hold on it, as the last initial
  // procedure of a program does by ending the program (LRM 24.3). Returns true
  // if the whole process ran to completion, false if it stopped to wait again.
  static auto Run(RuntimeEffects& effects, Activation* activation) -> bool;

  // Runs an execution which nothing outside it names, from `activation` in
  // place of whatever is executing, up to its next wait, as a C++ coroutine
  // that does not suspend initially runs: one not yet started, or one woken
  // inside the occurrence it waited for. An error it meets there stops the
  // process executing too, since it is met where that process stands.
  static void RunUndispatched(RuntimeEffects& effects, Activation* activation);

  // The top-level frame's activation. A wait is arranged for whichever frame is
  // carrying the thread, which is the innermost one; this is what the engine
  // schedules to start the process and what completion is judged against. Null
  // once the body has terminated.
  [[nodiscard]] auto TopActivation() const -> Activation*;

  // The processes this one started. A fork's parallel statement is a thread of
  // the process that executed the fork (LRM 9.5), which is the process running
  // when the branch is spawned, not the frame that spawned it.
  [[nodiscard]] auto Children() -> OwnedProcesses& {
    return children_;
  }

  // The process that started this one, while it keeps this one; null for one
  // no process started, and for one released.
  [[nodiscard]] auto Parent() const -> RuntimeProcess*;

  // This process's termination (LRM 9.3.2 `join`, 9.7 `await`): what a wait
  // for it enrols on. Whatever is parked there is asked once this process
  // settles any terminal state -- normal completion or a kill.
  [[nodiscard]] auto Termination() -> IntrusiveList<WaitMembership>& {
    return termination_;
  }

  // The waits that reached this process through a weak reference (LRM 8.30.3),
  // told once nothing refers to it any more -- which a process that has
  // terminated may still be referred to by long after.
  [[nodiscard]] auto Expiry() -> IntrusiveList<WaitMembership>& {
    return expiry_;
  }

  // Stops the frame carrying this process's thread at `awaiter`, and answers
  // whether the caller must give up control -- false where what it waits for
  // had already happened, in which case nothing was arranged and the caller
  // carries on.
  //
  // This is the one way a body stops to wait, which is what makes the wait
  // something the execution holds. A construct that reached the scheduler
  // without one would be one that process control could not restart (LRM 9.7),
  // and there is no way to spell that here.
  auto ParkAt(RuntimeEffects& services, Awaiter& awaiter) -> bool;

  // The frame of this thread that is blocked right now, if one is: what a
  // `disable` of a target this process is inside has to give control back to
  // (LRM 9.6.2). One running, already runnable, or stopped from outside is not
  // blocked, and reaches the check on its own when it next runs.
  [[nodiscard]] auto BlockedLeaf() const -> Activation*;

  // Hands this thread to `nested` and takes it back. A called task runs in its
  // caller's thread (LRM 9.5) rather than as a process of its own, so one
  // process can be inside several activations at once; the innermost is what a
  // stop this thread makes parks, and what the scheduler resumes. They are
  // owned here because nothing below them outlives a suspension.
  auto PushActivation(Coroutine<void> nested) -> Activation*;
  void PopActivation();

  // The same relation for a nested activation this process does not own,
  // because the frame that enabled it holds it. Which frame carries the thread
  // is the same fact either way, so it is recorded the same way.
  void EnterLeaf(Activation* leaf);
  void LeaveLeaf();

  // The run-time error that left the innermost activation's body, if any. A
  // driver written as a coroutine stores whatever leaves the body it drives,
  // because that is what the language does with an escaping exception; this
  // takes it back out so it can continue on its way to a landing.
  [[nodiscard]] auto TakeInnermostRaisedError() -> std::exception_ptr;

  [[nodiscard]] auto CurrentLeaf() const -> Activation* {
    return current_leaf_;
  }

  // Enters `fe` -- the foreign call this thread's innermost frame is making --
  // and drives it to its first suspension or its return. Returns true if the
  // call returned without suspending (the caller continues inline), false if it
  // suspended across the boundary (a frame inside it blocked, and the scheduler
  // resumes it later by driving the innermost call).
  auto EnterForeignExecution(std::unique_ptr<ForeignExecution> fe) -> bool;

  // Enter (LRM 9.6.2) a disable target: until the target is left, a `disable`
  // of it reaches this execution, landing where `landing` says, and a check
  // finds it among the targets this execution is inside.
  void PushEnclosingTarget(
      CancellationTarget* target, support::TargetLanding landing) {
    enclosing_targets_.emplace_back(
        CapturedTarget{.target = target, .generation = target->Generation()},
        landing, *this);
  }

  // Leave a target. Reached on every exit path an execution can take out of
  // it, including one already leaving under an effect, so it never raises:
  // raising from there would end the program rather than report anything.
  void PopEnclosingTarget(CancellationTarget* target) noexcept {
    if (!enclosing_targets_.empty() &&
        enclosing_targets_.back().captured.target == target) {
      enclosing_targets_.pop_back();
    }
  }

  // LRM 9.6.2, 9.7: whether a departure is owed here -- a target this execution
  // is inside was disabled, or its own termination was requested or settled.
  // The three are one question because what each owes this execution is the
  // same: it runs no further statement of the design.
  [[nodiscard]] auto DepartureIsDue() const -> bool {
    return TerminationIsDue() || OutermostInvalidatedTarget() != nullptr;
  }

  // LRM 9.7: whether this execution's own termination was requested or
  // settled, which nothing may continue past.
  [[nodiscard]] auto TerminationIsDue() const -> bool {
    return termination_requested_ ||
           execution_state_ == ProcessExecutionState::kTerminated;
  }

  // Whether a foreign call this execution made has still to return. Such an
  // execution cannot be torn down where it stands: the frames between it and
  // its own are another language's, and they end only by running.
  [[nodiscard]] auto HasLiveForeignCall() const -> bool {
    return !foreign_calls_.empty();
  }

  // The outermost target this execution is inside that has been disabled since
  // it entered (LRM 9.6.2), or null when none has. Outermost wins because
  // leaving a target also leaves every target nested within it.
  [[nodiscard]] auto OutermostInvalidatedTarget() const -> CancellationTarget* {
    for (const EnclosingTarget& enclosing : enclosing_targets_) {
      if (!enclosing.captured.Holds()) {
        return enclosing.captured.target;
      }
    }
    return nullptr;
  }

  // LRM 9.6.2 "all activities enabled within" a target: an activity spawned
  // inside a target is enabled within it, so a spawned process starts out
  // enclosed by exactly the targets its spawner was inside. Capturing them here
  // -- at the spawn, from the spawner's live state -- is what makes the spawned
  // activity a member from the instant it exists, before it has run any of its
  // own code, and reaches targets that are not in its body's lexical scope at
  // all (a fork inside a task called from the target). Nothing of the spawned
  // activity lies past any of them, so a `disable` of one ends it.
  void InheritEnclosingTargets(const RuntimeProcess& spawner) {
    for (const EnclosingTarget& enclosing : spawner.enclosing_targets_) {
      enclosing_targets_.emplace_back(
          enclosing.captured, support::TargetLanding::kAtExecutionEnd, *this);
    }
  }

  // LRM 9.7 `suspend`: withdraw the active leaf from whatever would resume it
  // and record the suspended state. What the leaf is waiting for stays with it,
  // because starting the process again waits for that same thing; only its
  // being parked there goes. A process already suspended, terminated, or on its
  // way to terminating is unaffected.
  void Suspend();

  // LRM 9.7 `resume`: a process that is not suspended is unaffected. Otherwise
  // its leaf waits again for what it was waiting for, and runs in the current
  // time step where that has already happened -- which is the whole of the
  // clause, because a leaf that was runnable when it was stopped is one whose
  // wait was already satisfied. A departure owed while it was stopped -- a
  // target it is inside was disabled -- is taken now, as it would have been
  // had it been blocked (LRM 9.6.2).
  void Resume(RuntimeEffects& effects);

  // The epoch a deferred report queued by this process right now belongs to
  // (LRM 16.4.1, 12.4.2.1), created on first use because most processes queue
  // none.
  [[nodiscard]] auto CurrentDeferredReportEpoch()
      -> std::shared_ptr<DeferredReportEpoch> {
    if (deferred_report_epoch_ == nullptr) {
      deferred_report_epoch_ = std::make_shared<DeferredReportEpoch>();
    }
    return deferred_report_epoch_;
  }

  // LRM 16.4.2 / 12.4.2.1: reaching a flush point clears this process's
  // deferred report queue, discarding every report still pending.
  void FlushDeferredReports() noexcept {
    if (deferred_report_epoch_ != nullptr) {
      deferred_report_epoch_->withdrawn = true;
      deferred_report_epoch_.reset();
    }
  }

  // The disable targets a report queued right now records (LRM 16.4.4). Two of
  // the targets this execution is inside can withdraw it and no other can: the
  // innermost, which is how a `disable` names one assertion, since a statement
  // label is a named block (LRM 16.3); and the outermost, whose disable is the
  // flush of the whole queue. Disabling a target between them cancels nothing,
  // so nothing between them is recorded.
  [[nodiscard]] auto DeferredReportTargets() const
      -> std::vector<CapturedTarget> {
    std::vector<CapturedTarget> recorded;
    if (enclosing_targets_.empty()) {
      return recorded;
    }
    recorded.push_back(enclosing_targets_.front().captured);
    if (enclosing_targets_.size() > 1) {
      recorded.push_back(enclosing_targets_.back().captured);
    }
    return recorded;
  }

  // Bulk subtree termination (`kill` and `disable fork`) dismantles the whole
  // subtree, waking what awaits each node's termination, and its parent's
  // `wait fork`, as that node settles. Waking only queues -- nothing runs until
  // the region the activation was queued in does, which is after the
  // dismantling is over -- and an activation woken here whose own frame a later
  // step of the dismantling releases leaves its queue with that frame, so
  // nothing is left to resume it.
  //
  // Forcibly terminates every descendant of this process -- not only its
  // immediate children, and including the descendants of subprocesses that have
  // already terminated (LRM 9.6.3 `disable fork`). This process itself keeps
  // running. Shares the kill primitive: each newly-terminated descendant is
  // marked KILLED and its frame released, so no queue or wait can name it, and
  // leaves this lineage once nothing under it is left. A descendant kept alive
  // by a `process` handle survives that as a parent-less terminal node
  // reporting KILLED; the rest are reclaimed. One whose kill could only be
  // requested stays in place until it settles.
  void DisableDescendants(RuntimeEffects& services);

  // Forcibly terminates this process and its whole subtree (LRM 9.7 `kill`):
  // each node not already terminal is marked KILLED and its frame released, and
  // every descendant is dismantled as `disable fork` dismantles them. This node
  // itself stays with whoever started it; the caller lets the lineage go.
  void TerminateSubtreeKilled(RuntimeEffects& services);

  // Terminates this subtree (LRM 9.7 `kill` / LRM 9.6 `disable`) when it
  // contains `running` -- the calling process, whose frame is executing and so
  // cannot be destroyed synchronously. Every off-path subtree is killed and
  // dismantled synchronously as usual; the single chain of nodes from this one
  // down to `running` is settled but kept, so `running` stays owned until it
  // unwinds to its safe boundary. `running` itself is only
  // termination-requested (phase 1), not settled: its terminal state is
  // published, and the whole retained chain released, when its body reaches the
  // resume boundary. The caller unwinds `running` after this returns.
  void TerminateSubtreeDeferringRunning(
      RuntimeProcess& running, RuntimeEffects& services);

  // Whether `other` is this process or a descendant of it -- i.e. whether
  // terminating this subtree would tear down `other`'s frame. `kill` consults
  // it against the calling process to route it to the deferred self / ancestor
  // path.
  [[nodiscard]] auto IsSelfOrAncestorOf(const RuntimeProcess& other) const
      -> bool;

  // Phase 1 of a deferred, safe-boundary termination (LRM 9.6 / 9.7): the
  // calling process cannot destroy the frame it is running in, so termination
  // is split. This records the terminal cause and takes the leaf off whatever
  // could resume it -- nothing can wake it -- but does NOT publish the terminal
  // state or wake what awaits the termination, because the body is still going
  // to run its unwind. The terminal state, frame release, and that waking
  // happen atomically later (phase 2), when the body has unwound to a safe
  // boundary.
  // Publishing nothing here is the point: an observer of `status()` or a
  // termination waiter never sees the process terminated while its body may
  // still run user code. Exactly-once -- a re-request on an already-requested
  // or terminated process is a no-op.
  void RequestTermination(ProcessTerminationCause cause);

  // Forcibly terminates `process` and its whole subtree (LRM 9.7 `kill`, and a
  // `disable` of the statement it executes, LRM 9.6.3), releasing what that
  // leaves with nothing under it. Where `process` is the executing one or an
  // ancestor of it, the executing one is only asked to stop, and leaves where
  // it next asks whether a departure is due. The caller holds `process`, so
  // releasing it cannot free it mid-call.
  static void Kill(RuntimeProcess& process, RuntimeEffects& services);

  // Releases `process` where it has terminated with nothing under it, then each
  // ancestor that release leaves the same way, walking upward, so each is
  // destroyed unless a `process` handle (LRM 9.7) still holds it. Deliberately
  // not a call on the released node: the release destroys it.
  static void ReleaseTerminatedLineage(RuntimeProcess& process);

 private:
  // Resumes `activation` -- the specific frame that suspended (the innermost
  // one when a task enabled by this process is suspended). Symmetric transfer
  // carries control back up the enable chain. Returns true if the whole process
  // ran to completion (judged on the top-level frame), false if it stopped to
  // wait again. `activation` may be destroyed by the time this returns, so
  // completion and exceptions are read off the top-level frame, not it.
  //
  // Completing runs the whole terminal transition atomically -- terminal state,
  // frame release, and waking what awaits this process's termination -- so a
  // body can never be left terminated with its waiters unwoken. The node
  // outlives the frame. Taking the runtime is what makes this the only way a
  // body can run: the resumed body reaches its own process identity through
  // the runtime's ambient, and a caller cannot resume without supplying it.
  auto ResumeWith(RuntimeEffects& effects, Activation* activation) -> bool;

  // The set that keeps a process is what places and moves it.
  friend class OwnedProcesses;

  // One disable target this execution is inside (LRM 9.6.2): the generation it
  // captured entering, and its membership on the target, which stands for as
  // long as this record does. Being blocked inside a target is waiting for it
  // not to be disabled, alongside whatever else the frame waits for, and the
  // membership is how a `disable` finds the process to ask.
  struct EnclosingTarget {
    EnclosingTarget(
        CapturedTarget captured, support::TargetLanding landing,
        RuntimeProcess& inside)
        : captured(captured) {
      membership.process = &inside;
      membership.landing = landing;
      captured.target->Members().PushBack(membership);
    }

    CapturedTarget captured;
    TargetMembership membership;
  };

  // Drives `fe` for one stretch -- into its next suspension or its return --
  // with this process installed as the one whose foreign call is running, so an
  // exported subroutine the call reaches runs as it. Returns whether
  // the call has returned. Every drive goes through here so that installation
  // can never be forgotten.
  auto DriveForeignVehicle(ForeignExecution& fe) -> bool;

  // The one indivisible terminal transition, and the sole writer of the
  // terminal state: a body can never be marked terminated while it still owns
  // the frame it ran in, nor terminated without what awaits it being asked.
  // Sets terminal state + cause, releases the frame (which ends every wait it
  // held and its place in any queue), wakes what awaits this process's
  // termination, and tells whoever keeps it that it has ended, which asks that
  // owner's `wait fork` (LRM 9.6.1).
  void SettleTerminated(
      ProcessTerminationCause cause, RuntimeEffects& services);
  // Ends this node, or -- where its execution holds a foreign call that has yet
  // to return -- asks it to end and hands it control once more so that call can
  // return. Every bulk termination goes through here, so no path can settle a
  // node whose frame is under another language's.
  void SettleOrRequestKilled(RuntimeEffects& services);
  // Parks `leaf` at `awaiter` and arranges for it to carry on as `resumption`
  // says, answering whether it is parked -- false where it carries on without
  // stopping, in which case nothing was arranged.
  static auto Arrange(
      RuntimeEffects& services, Activation* leaf, Awaiter& awaiter,
      const Resumption& resumption) -> bool;
  [[nodiscard]] auto IsReleasable() const -> bool;
  // Has whoever keeps this process stop keeping it, which destroys it where
  // nothing else holds it; so nothing may touch this process after the call.
  void Release();
  // Kills this process with its subtree, and releases it where that leaves
  // nothing under it. Only this process is released, never an ancestor: the
  // caller is walking its own children, as is every ancestor above it, and
  // each releases its child once that walk returns.
  void Dismantle(RuntimeEffects& services);

  ExecutionContext context_;
  Coroutine<void> coroutine_;
  RunningState running_;
  // The frame the engine will resume next for this process (invariant: a
  // non-executing process has exactly one active leaf). Starts at the top frame
  // and follows the innermost parked frame as waits block it.
  Activation* current_leaf_ = nullptr;
  // The activations this thread is inside beyond its own body, innermost last,
  // each called by the one before it. Empty while it runs its own body. Only
  // the activations this process owns are here; one a calling frame holds is
  // recorded in `outer_leaves_` alone.
  std::vector<Coroutine<void>> nested_activations_;
  // What `current_leaf_` was before each activation this thread entered, so
  // leaving one restores the frame that called it. Innermost last, the same
  // order as the activations themselves.
  std::vector<Activation*> outer_leaves_;
  // The vehicles of the foreign calls this thread has entered and not yet
  // returned from, innermost last; the last is what the scheduler drives to
  // resume a thread blocked inside them. When one returns, the frame that made
  // it is the thread's innermost frame again, since every exported subroutine
  // it reached has left the frame it entered. Each is held until its own call
  // returns, which is after the frame that made it has parked, so nothing
  // shorter-lived can own one.
  std::vector<std::unique_ptr<ForeignExecution>> foreign_calls_;
  // Held by every deferred report this process has pending; null when it has
  // none, which is also the state a flush point leaves it in.
  std::shared_ptr<DeferredReportEpoch> deferred_report_epoch_;
  ProcessExecutionState execution_state_ = ProcessExecutionState::kCreated;
  ProcessTerminationCause termination_cause_ =
      ProcessTerminationCause::kCompleted;
  // Set when a deferred termination is requested (phase 1) and consumed at the
  // resume boundary (phase 2): pending while the body unwinds.
  bool termination_requested_ = false;
  // Where whoever keeps this process holds it, and which set that is; null
  // once this process has left it.
  OwnedPlace owned_place_;
  OwnedProcesses* owned_by_ = nullptr;
  OwnedProcesses children_{this};
  // The disable targets enclosing this execution, outermost first: those
  // inherited from the spawner, then those entered by this execution's own
  // frames. Entering a target pushes it and leaving pops it. Each is on its
  // own because its target's list points at the membership it holds, and most
  // processes are inside none.
  std::list<EnclosingTarget> enclosing_targets_;
  // Every wait for this process to terminate -- a `join` of the fork that
  // spawned it, an `await` of it (LRM 9.3.2, 9.7) -- asked when this process
  // settles a terminal state.
  IntrusiveList<WaitMembership> termination_;
  IntrusiveList<WaitMembership> expiry_;
};

}  // namespace lyra::runtime
