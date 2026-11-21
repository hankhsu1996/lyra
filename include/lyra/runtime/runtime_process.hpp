#pragma once

#include <algorithm>
#include <cstdint>
#include <exception>
#include <memory>
#include <utility>
#include <vector>

#include "lyra/runtime/cancellation.hpp"
#include "lyra/runtime/class_object.hpp"
#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/intrusive_list.hpp"
#include "lyra/runtime/owned_processes.hpp"
#include "lyra/runtime/rng.hpp"
#include "lyra/runtime/running_state.hpp"
#include "lyra/runtime/wait.hpp"
#include "lyra/support/combinational_updates.hpp"

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
  // `disable` (LRM 9.6), the run it ended (LRM 20.2), or a run-time error the
  // body had no way to carry on from. Reported as KILLED.
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
    return (pass == nullptr || !pass->withdrawn) &&
           std::ranges::all_of(targets, [](const CapturedTarget& captured) {
             return captured.Holds();
           });
  }
};

// What executes a process -- the frames of a thread, or a reaction. It is
// something that runs, and it answers what process control asks of it besides:
// whether it is running, how it is released, whether a foreign call holds it,
// and what of its running state outlives a stop. Its leaf is not asked once it
// is released.
class ProcessExecution : public Runner {
 public:
  ProcessExecution();
  ProcessExecution(const ProcessExecution&) = delete;
  auto operator=(const ProcessExecution&) -> ProcessExecution& = delete;
  ProcessExecution(ProcessExecution&&) = delete;
  auto operator=(ProcessExecution&&) -> ProcessExecution& = delete;
  ~ProcessExecution() override;

  // Whether it has still to start, is running, or waits, which is what
  // `status()` reports of its process while that is neither suspended nor
  // ended (LRM 9.7).
  [[nodiscard]] virtual auto Activity() const -> ProcessExecutionState = 0;

  // Releases it, once its process has terminated: nothing of it runs or is
  // resumed again, and every wait it held ends.
  virtual void ReleaseExecution() = 0;

  // Whether a foreign call it made has still to return. Such an execution
  // cannot be torn down where it stands: the frames between it and its own are
  // another language's, and they end only by running.
  [[nodiscard]] virtual auto HasLiveForeignCall() const -> bool = 0;

  // What is in force for it that outlives a stop: the disable targets it is
  // inside and whether it ended the run. None where nothing of it outlives
  // one.
  [[nodiscard]] virtual auto HeldState() const -> const RunningState* = 0;

  // Takes on the dialect the wait of the `always_comb` or `always_latch` it
  // executes states, as that procedure begins (LRM 9.2.2.2, 9.2.2.3).
  virtual void SetCombinationalUpdates(
      support::CombinationalUpdates updates) = 0;
};

// A process (LRM 9.5, 9.7): a node of the dynamic process lineage, the object
// the language names it by, and what process control acts on. What executes
// it is not part of it, and is reached only through what that execution
// answers, so process control is written once, here.
//
// The node outlives its execution: the execution is released the moment the
// body terminates, whereas the node survives for as long as any descendant is
// still alive. LRM 9.6.3 demands the separation -- `disable fork` terminates
// the descendants of subprocesses that have already terminated, so a
// terminated process stays reachable from its parent.
//
// A process's children are those it spawned itself, whether directly or from a
// task or function it called (LRM 9.6.1): a subroutine call runs in the
// caller's thread and creates no process of its own. They therefore accumulate
// across every fork the process executes.
//
// A process is an object of a class (LRM 9.7), so it starts with the part
// every class object does: its class's definition, through which the library
// does to it whatever it does to an object whose class it does not know, and
// its generator. The object's generator is the thread's, which the process's
// `srandom()` seeds (LRM 18.13.3); there is no other. Its storage starts at
// that part, which is how an object is reached from generated code and from a
// handle, so it has no virtual function of its own.
class RuntimeProcess : public ClassObject {
 public:
  // The process `execution` is, which executes it for as long as it runs.
  [[nodiscard]] static auto Create(
      ProcessExecution& execution, DrawRng generator)
      -> std::shared_ptr<RuntimeProcess>;

  RuntimeProcess(const RuntimeProcess&) = delete;
  auto operator=(const RuntimeProcess&) -> RuntimeProcess& = delete;
  // Non-movable: what executes it and every child node point back at it, so
  // its address must stay stable for as long as either can name it. A node is
  // retained by shared ownership -- whoever started it while it or a descendant
  // is live, and any user-visible `process` handle (LRM 9.7) -- so its lifetime
  // is reachability, not a single owner.
  RuntimeProcess(RuntimeProcess&&) noexcept = delete;
  auto operator=(RuntimeProcess&&) noexcept -> RuntimeProcess& = delete;
  ~RuntimeProcess();

  // What its execution runs in, where process control reads what created its
  // thread before it acts (LRM 9.7). Read by a run stopped from outside, so it
  // may not throw.
  [[nodiscard]] auto Context() const noexcept -> const ExecutionContext& {
    return execution_->Context();
  }

  // The process's execution state: what process control left it in, and
  // otherwise what its execution is doing. A `process` handle (LRM 9.7) reads
  // it through `status()`, which projects it onto the LRM state enum; it
  // survives the execution, so a terminated process is still observable.
  [[nodiscard]] auto ExecutionState() const -> ProcessExecutionState {
    if (terminated_) {
      return ProcessExecutionState::kTerminated;
    }
    if (suspended_) {
      return ProcessExecutionState::kSuspended;
    }
    return execution_->Activity();
  }

  // Why the process terminated -- meaningful only once `ExecutionState()` is
  // `kTerminated`. Distinguishes a FINISHED process from a KILLED one for
  // `status()` (LRM 9.7).
  [[nodiscard]] auto TerminationCause() const -> ProcessTerminationCause {
    return termination_cause_;
  }

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

  // The activation of this thread that is blocked right now, if one is: what
  // a `disable` of a target this process is inside has to give control back
  // to (LRM 9.6.2). One running, already runnable, or stopped from outside is
  // not blocked, and reaches the check on its own when it next runs.
  [[nodiscard]] auto BlockedLeaf() -> Activation*;

  // LRM 9.6.2, 9.7, 20.2: whether a departure is owed here -- a target this
  // execution is inside was disabled, its own termination was requested or
  // settled, or it ended the run. They are one question because what each owes
  // this execution is the same: it runs no further statement of the design.
  [[nodiscard]] auto DepartureIsDue() const -> bool;

  // LRM 20.2: whether this execution ended the run, after which nothing of the
  // design runs in it again, whether or not it has settled yet.
  [[nodiscard]] auto EndedTheRun() const -> bool;

  // LRM 9.7: whether this execution's own termination was requested or
  // settled, which nothing may continue past.
  [[nodiscard]] auto TerminationIsDue() const -> bool {
    return termination_requested_ || terminated_;
  }

  // What is in force for its execution that outlives a stop, the disable
  // targets it is inside among it; none where nothing of it outlives one.
  [[nodiscard]] auto HeldState() const -> const RunningState* {
    return execution_->HeldState();
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

  // Bulk subtree termination (`kill` and `disable fork`) dismantles the whole
  // subtree, waking what awaits each node's termination, and its parent's
  // `wait fork`, as that node settles. Waking only queues -- nothing runs until
  // the region the activation was queued in does, which is after the
  // dismantling is over -- and an activation woken here whose own execution a
  // later step of the dismantling releases leaves its queue with it, so
  // nothing is left to resume it.
  //
  // Forcibly terminates every descendant of this process -- not only its
  // immediate children, and including the descendants of subprocesses that have
  // already terminated (LRM 9.6.3 `disable fork`). This process itself keeps
  // running. Shares the kill primitive: each newly-terminated descendant is
  // marked KILLED and its execution released, so no queue or wait can name it,
  // and leaves this lineage once nothing under it is left. A descendant kept
  // alive by a `process` handle survives that as a parent-less terminal node
  // reporting KILLED; the rest are reclaimed. One whose kill could only be
  // requested stays in place until it settles.
  void DisableDescendants(RuntimeEffects& services);

  // Forcibly terminates this process and its whole subtree (LRM 9.7 `kill`):
  // each node not already terminal is marked KILLED and its execution
  // released, and every descendant is dismantled as `disable fork` dismantles
  // them. This node itself stays with whoever started it; the caller lets the
  // lineage go.
  void TerminateSubtreeKilled(RuntimeEffects& services);

  // Terminates this subtree (LRM 9.7 `kill` / LRM 9.6 `disable`) when it
  // contains `running` -- the calling process, whose execution is running and
  // so cannot be released synchronously. Every off-path subtree is killed and
  // dismantled synchronously as usual; the single chain of nodes from this one
  // down to `running` is settled but kept, so `running` stays owned until it
  // unwinds to its safe boundary. `running` itself is only
  // termination-requested (phase 1), not settled: its terminal state is
  // published, and the whole retained chain released, when its body reaches the
  // resume boundary. The caller unwinds `running` after this returns.
  void TerminateSubtreeDeferringRunning(
      RuntimeProcess& running, RuntimeEffects& services);

  // Whether `other` is this process or a descendant of it -- i.e. whether
  // terminating this subtree would tear down `other`'s execution. `kill`
  // consults it against the calling process to route it to the deferred self /
  // ancestor path.
  [[nodiscard]] auto IsSelfOrAncestorOf(const RuntimeProcess& other) const
      -> bool;

  // Phase 1 of a deferred, safe-boundary termination (LRM 9.6 / 9.7): the
  // calling process cannot release the execution it is running in, so
  // termination is split. This records the terminal cause and takes the leaf
  // off whatever could resume it -- nothing can wake it -- but does NOT publish
  // the terminal state or wake what awaits the termination, because the body
  // is still going to run its unwind. The terminal state, the release, and
  // that waking happen atomically later (phase 2), when the body has unwound
  // to a safe boundary.
  // Publishing nothing here is the point: an observer of `status()` or a
  // termination waiter never sees the process terminated while its body may
  // still run user code. Exactly-once -- a re-request on an already-requested
  // or terminated process is a no-op.
  void RequestTermination(ProcessTerminationCause cause);

  // Forcibly terminates `process` and its whole subtree (LRM 9.7 `kill`, and a
  // `disable` of a target it is an activity enabled within, LRM 9.6.2),
  // releasing what that leaves with nothing under it. Where `process` is the
  // executing one or an ancestor of it, the executing one is only asked to
  // stop, and leaves where it next asks whether a departure is due. The caller
  // holds `process`, so releasing it cannot free it mid-call.
  static void Kill(RuntimeProcess& process, RuntimeEffects& services);

  // Releases `process` where it has terminated with nothing under it, then each
  // ancestor that release leaves the same way, walking upward, so each is
  // destroyed unless a `process` handle (LRM 9.7) still holds it. Deliberately
  // not a call on the released node: the release destroys it.
  static void ReleaseTerminatedLineage(RuntimeProcess& process);

  // A reference to this process, owning what every other reference to it owns.
  [[nodiscard]] auto Shared() -> std::shared_ptr<RuntimeProcess>;

  // Parks `leaf` at `awaiter` and arranges for it to carry on as `resumption`
  // says, answering whether it is parked -- false where it carries on without
  // stopping, in which case nothing was arranged.
  static auto Arrange(
      RuntimeEffects& services, Activation* leaf, Awaiter& awaiter,
      const Resumption& resumption) -> bool;

  // The one indivisible terminal transition, and the sole writer of the
  // terminal state: a body can never be marked terminated while it still owns
  // the execution it ran in, nor terminated without what awaits it being
  // asked. Sets terminal state + cause, releases the execution (which ends
  // every wait it held and its place in any queue), wakes what awaits this
  // process's termination, and tells whoever keeps it that it has ended, which
  // asks that owner's `wait fork` (LRM 9.6.1).
  void SettleTerminated(
      ProcessTerminationCause cause, RuntimeEffects& services);

 protected:
  // An object ended as `definition` says, which is how its own class ends.
  RuntimeProcess(
      const abi::ObjectDefinition& definition, ProcessExecution& execution,
      DrawRng generator);

 private:
  // The set that keeps a process is what places and moves it.
  friend class OwnedProcesses;

  // Ends this node, or -- where its execution holds a foreign call that has yet
  // to return -- asks it to end and hands it control once more so that call can
  // return. Every bulk termination goes through here, so no path can settle a
  // node whose frame is under another language's.
  void SettleOrRequestKilled(RuntimeEffects& services);
  [[nodiscard]] auto IsReleasable() const -> bool;
  // Has whoever keeps this process stop keeping it, which destroys it where
  // nothing else holds it; so nothing may touch this process after the call.
  void Release();
  // Kills this process with its subtree, and releases it where that leaves
  // nothing under it. Only this process is released, never an ancestor: the
  // caller is walking its own children, as is every ancestor above it, and
  // each releases its child once that walk returns.
  void Dismantle(RuntimeEffects& services);

  ProcessExecution* execution_;
  // Held by every deferred report this process has pending; null when it has
  // none, which is also the state a flush point leaves it in.
  std::shared_ptr<DeferredReportEpoch> deferred_report_epoch_;
  bool suspended_ = false;
  bool terminated_ = false;
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
  // Every wait for this process to terminate -- a `join` of the fork that
  // spawned it, an `await` of it (LRM 9.3.2, 9.7) -- asked when this process
  // settles a terminal state.
  IntrusiveList<WaitMembership> termination_;
};

// A thread that is a stack of coroutine frames, which it owns while its body
// runs: its body, the tasks it has called (LRM 9.5), and the foreign calls it
// is inside (LRM 35.5). It is what executes its process, and what runs while
// one of its frames does.
class CoroutineThread final : public ProcessExecution {
 public:
  CoroutineThread(
      RuntimeProcess& process, ExecutionContext context,
      Coroutine<void> coroutine);
  CoroutineThread(const CoroutineThread&) = delete;
  auto operator=(const CoroutineThread&) -> CoroutineThread& = delete;
  CoroutineThread(CoroutineThread&&) = delete;
  auto operator=(CoroutineThread&&) -> CoroutineThread& = delete;
  // Ends the foreign calls still suspended when the run ends, innermost first,
  // as nested extents end, because a call made inside another is part of it.
  // Out of line, because the vehicle that carries one is only named here.
  ~CoroutineThread() override;

  // What is in force while this thread runs: the generator its randomization
  // calls draw from and a process spawned from here takes its seed out of (LRM
  // 18.14.2), which is its process's own, and the DPI scope chain its foreign
  // calls report through (LRM 35.5.3). It belongs to the thread rather than to
  // the run so that two foreign calls suspended on different threads never
  // share a chain.
  [[nodiscard]] auto State() -> RunningState& override {
    return running_;
  }

  [[nodiscard]] auto Context() const noexcept
      -> const ExecutionContext& override {
    return context_;
  }

  [[nodiscard]] auto Leaf() -> FrameActivation* override {
    return current_leaf_;
  }

  [[nodiscard]] auto ProcessIfMade() -> RuntimeProcess* override {
    return process_;
  }
  [[nodiscard]] auto Process() -> RuntimeProcess* override {
    return process_;
  }
  void SetCombinationalUpdates(support::CombinationalUpdates updates) override {
    context_.combinational_updates = updates;
  }

  // The final procedures are calls one thread makes in turn (LRM 9.2.3), each
  // reporting from its own scope and place and seeding from its own instance.
  // Takes on `context` and answers the one it replaces, for the call's end to
  // restore; nothing the thread itself is changes.
  [[nodiscard]] auto EnterProcedure(const ExecutionContext& context)
      -> ExecutionContext {
    return std::exchange(context_, context);
  }
  void LeaveProcedure(const ExecutionContext& left) {
    context_ = left;
  }

  // The top-level frame's activation, which is what the engine schedules to
  // start the thread and what completion is judged against. Null once the
  // body has terminated.
  [[nodiscard]] auto TopActivation() const -> FrameActivation*;

  // Hands this thread to `nested` and takes it back. A called task runs in its
  // caller's thread (LRM 9.5) rather than as a process of its own, so one
  // thread can be inside several activations at once; the innermost is what a
  // stop this thread makes parks, and what the scheduler resumes. They are
  // owned here because nothing below them outlives a suspension.
  auto PushActivation(Coroutine<void> nested) -> FrameActivation*;
  void PopActivation();

  // The same relation for a nested activation this thread does not own,
  // because the frame that enabled it holds it. Which frame carries the thread
  // is the same fact either way, so it is recorded the same way.
  void EnterLeaf(FrameActivation* leaf);
  void LeaveLeaf();

  // The run-time error that left the innermost activation's body, if any. A
  // driver written as a coroutine stores whatever leaves the body it drives,
  // because that is what the language does with an escaping exception; this
  // takes it back out so it can continue on its way to a landing.
  [[nodiscard]] auto TakeInnermostRaisedError() -> std::exception_ptr;

  // Enters `fe` -- the foreign call this thread's innermost frame is making --
  // and drives it to its first suspension or its return. Returns true if the
  // call returned without suspending (the caller continues inline), false if it
  // suspended across the boundary (a frame inside it blocked, and the scheduler
  // resumes it later by driving the innermost call).
  auto EnterForeignExecution(std::unique_ptr<ForeignExecution> fe) -> bool;

  // Releasing a frame destroys it, and with it every wait it held and its place
  // in any queue.
  void ReleaseExecution() override;
  [[nodiscard]] auto HasLiveForeignCall() const -> bool override {
    return !foreign_calls_.empty();
  }
  [[nodiscard]] auto HeldState() const -> const RunningState* override {
    return &running_;
  }
  [[nodiscard]] auto Activity() const -> ProcessExecutionState override {
    return activity_;
  }

 private:
  // A thread is resumed only by a run of one of its frames, which holds its
  // process for as long as the resume lasts.
  friend struct FrameActivation;

  // Resumes `activation` -- the specific frame that suspended (the innermost
  // one when a task enabled by this thread is suspended). Symmetric transfer
  // carries control back up the enable chain. Returns true if the whole process
  // ran to completion (judged on the top-level frame), false if it stopped to
  // wait again. `activation` may be destroyed by the time this returns, so
  // completion and exceptions are read off the top-level frame, not it.
  //
  // Completing runs the whole terminal transition atomically -- terminal state,
  // frame release, and waking what awaits the process's termination -- so a
  // body can never be left terminated with its waiters unwoken. The process
  // outlives the frame. Taking the runtime is what makes this the only way a
  // body can run: the resumed body reaches its own process through the
  // runtime's ambient, and a caller cannot resume without supplying it.
  auto ResumeWith(RuntimeEffects& effects, FrameActivation* activation) -> bool;

  // Drives `fe` for one stretch -- into its next suspension or its return --
  // with this thread installed as the one whose foreign call is running, so an
  // exported subroutine the call reaches runs in it. Returns whether the call
  // has returned. Every drive goes through here so that installation can never
  // be forgotten.
  auto DriveForeignVehicle(ForeignExecution& fe) -> bool;

  RuntimeProcess* process_;
  ExecutionContext context_;
  Coroutine<void> coroutine_;
  RunningState running_;
  ProcessExecutionState activity_ = ProcessExecutionState::kCreated;
  // The frame the engine will resume next for this thread (invariant: a
  // thread not running has exactly one active leaf). Starts at the top frame
  // and follows the innermost parked frame as waits block it.
  FrameActivation* current_leaf_ = nullptr;
  // The activations this thread is inside beyond its own body, innermost last,
  // each called by the one before it. Empty while it runs its own body. Only
  // the activations this thread owns are here; one a calling frame holds is
  // recorded in `outer_leaves_` alone.
  std::vector<Coroutine<void>> nested_activations_;
  // What `current_leaf_` was before each activation this thread entered, so
  // leaving one restores the frame that called it. Innermost last, the same
  // order as the activations themselves.
  std::vector<FrameActivation*> outer_leaves_;
  // The vehicles of the foreign calls this thread has entered and not yet
  // returned from, innermost last; the last is what the scheduler drives to
  // resume a thread blocked inside them. When one returns, the frame that made
  // it is the thread's innermost frame again, since every exported subroutine
  // it reached has left the frame it entered. Each is held until its own call
  // returns, which is after the frame that made it has parked, so nothing
  // shorter-lived can own one.
  std::vector<std::unique_ptr<ForeignExecution>> foreign_calls_;
};

// A process executed by a thread of coroutine frames, which it holds: made
// with the thread, as a fork, a static process with a body that can stop, or
// an execution handed over to run where no thread runs it.
class CoroutineProcess final : public RuntimeProcess {
 public:
  // A process created whole, held by the reference its creator takes.
  // `generator` is the thread's own. LRM 18.14.1 fixes where it comes from by
  // how the process was created: a dynamically created one takes it out of the
  // process that created it, and a static process out of the initialization
  // RNG of its context's instance. One handed over to run where no thread runs
  // it -- an assertion's action, an update waiting on an event -- takes the one
  // what handed it over derives for it, and an action the end of the run
  // starts, which nothing hands over, the next of that RNG.
  [[nodiscard]] static auto Create(
      ExecutionContext context, Coroutine<void> coroutine, DrawRng generator)
      -> std::shared_ptr<CoroutineProcess>;

  CoroutineProcess(const CoroutineProcess&) = delete;
  auto operator=(const CoroutineProcess&) -> CoroutineProcess& = delete;
  CoroutineProcess(CoroutineProcess&&) noexcept = delete;
  auto operator=(CoroutineProcess&&) noexcept -> CoroutineProcess& = delete;
  ~CoroutineProcess();

  [[nodiscard]] auto Thread() -> CoroutineThread& {
    return thread_;
  }

 private:
  CoroutineProcess(
      ExecutionContext context, Coroutine<void> coroutine, DrawRng generator);

  CoroutineThread thread_;
};

}  // namespace lyra::runtime
