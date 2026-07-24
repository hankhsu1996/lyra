#pragma once

#include <cstdint>
#include <memory>
#include <vector>

#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/pending_wait.hpp"
#include "lyra/runtime/process_kind.hpp"
#include "lyra/runtime/registration.hpp"

namespace lyra::runtime {

class RuntimeEffects;
class ForeignExecution;
class ForeignExecutionGuard;
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
  // Ran to the end of its body -- normally or via an unhandled fault. LRM 9.7
  // reports this as FINISHED.
  kCompleted,
  // Forcibly terminated by `kill` (LRM 9.7) or `disable` (LRM 9.6). Reported as
  // KILLED.
  kKilled,
};

// The sentinel that unwinds the calling process's own body to the engine's
// resume boundary. A running frame cannot be destroyed synchronously, so `kill`
// / `disable` targeting the calling process records the terminal cause and
// throws this: the body's destructors run and control returns to the resume
// boundary, where the terminal state is published and the frame released.
//
// It is structured control flow, not an error: no base, no message, thrown only
// by `UnwindForProcessTermination` after a termination request is recorded, and
// consumed only at the resume boundary under that pending request. It travels
// the coroutine's completion channel (the promise's unhandled-exception slot),
// not a `catch` around a resume. It must never cross a boundary the runtime
// does not own: a foreign (`extern "C"`) frame has no C++ unwind contract, so a
// termination that must pass through foreign code uses the LRM cooperative
// return protocol to reach a simulator-owned boundary first, and only unwinds
// there.
struct ProcessTerminationUnwind final {};

// The sole throw site for the termination sentinel, so every delivery is one
// auditable point. Call only after the termination cause has been recorded.
[[noreturn]] inline void UnwindForProcessTermination() {
  throw ProcessTerminationUnwind{};
}

// A node of the dynamic process lineage (LRM 9.5) and, while its body runs, the
// owner of the coroutine frame executing it. The two lifetimes are distinct:
// the frame is released the moment the body terminates, whereas the node
// survives for as long as any descendant is still alive. LRM 9.6.3 demands the
// separation -- `disable fork` terminates the descendants of subprocesses that
// have already terminated, so a terminated process stays reachable from its
// parent. The child list consequently holds terminated nodes alongside running
// ones.
//
// A process's children are those it spawned itself, whether directly or from a
// task or function it called (LRM 9.6.1): a subroutine call runs in the
// caller's thread and creates no process of its own. The list therefore
// accumulates across every fork the process executes.
class Scope;

class RuntimeProcess : public std::enable_shared_from_this<RuntimeProcess> {
 public:
  // `owning_scope` is the scope this process was registered against -- the
  // scope whose Observed queue a runtime side effect this process reaches
  // attributes to. Every side effect the body performs runs in this scope's
  // attribution: an unqualified-case runtime warning, an NBA or postponed
  // submit, a `%m` hierarchical name. A fork branch inherits its parent
  // process's owning scope so a side effect the branch performs attributes
  // to the same scope the outer body's would. The registration site owns
  // the process's shared handle; the process's `owning_scope_` is a
  // borrowed pointer whose lifetime is that of the owning scope.
  RuntimeProcess(
      Scope* owning_scope, ProcessKind kind, Coroutine<void> coroutine);

  RuntimeProcess(const RuntimeProcess&) = delete;
  auto operator=(const RuntimeProcess&) -> RuntimeProcess& = delete;
  // Non-movable: the coroutine promise stores a back-pointer to this
  // RuntimeProcess, and so does every child node, so its address must stay
  // stable for as long as either can name it. A node is retained by shared
  // ownership -- its parent's lineage while a descendant is live, and any
  // user-visible `process` handle (LRM 9.7) -- so its lifetime is reachability,
  // not a single owner.
  RuntimeProcess(RuntimeProcess&&) noexcept = delete;
  auto operator=(RuntimeProcess&&) noexcept -> RuntimeProcess& = delete;
  ~RuntimeProcess() = default;

  [[nodiscard]] auto Kind() const -> ProcessKind;

  // The scope this process's runtime side effects attribute to. Inherited
  // by fork branches so a branch's effects belong to the same scope as the
  // outer body's would.
  [[nodiscard]] auto OwningScope() const -> Scope* {
    return owning_scope_;
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

  // Resumes `handle` -- the specific coroutine frame that suspended (the
  // innermost one when a task enabled by this process is suspended). Symmetric
  // transfer carries control back up the enable chain. Returns true if the
  // whole process ran to completion (judged on the top-level coroutine), false
  // if it suspended again on some awaitable. Captured `handle` may be destroyed
  // by the time this returns, so completion and exceptions are read off the
  // top-level coroutine, not `handle`.
  //
  // Completing runs the whole terminal transition atomically -- terminal state,
  // frame release, and draining this process's own `await` waiters into `woken`
  // -- so a body can never be left terminated with its waiters unwoken. The
  // node outlives the frame. Taking the runtime is what makes this the only
  // way a body can run: the resumed body reaches its own process identity
  // through the runtime's ambient, and a caller cannot resume without
  // supplying it.
  auto ResumeWith(
      RuntimeEffects& effects, CoroutineHandle handle,
      std::vector<CoroutineHandle>& woken) -> bool;

  // The top-level coroutine frame. Awaitables register the innermost handle for
  // wakeup; this is what the engine schedules to start the process and what
  // completion is judged against. Null once the body has terminated.
  [[nodiscard]] auto TopHandle() const -> CoroutineHandle;

  // Takes `child` into this process's lineage. A fork's parallel statement is a
  // thread of the process that executed the fork (LRM 9.5), which is the
  // process running when the branch is spawned, not the frame that spawned it.
  void AdoptChild(std::shared_ptr<RuntimeProcess> child);

  [[nodiscard]] auto Parent() const -> RuntimeProcess*;

  // Parks `waiter` on this process's `wait fork` condition (LRM 9.6.1). The
  // waiter is the frame that executed `wait fork` -- in a task it is the task
  // frame, not this process's own body -- so process identity and the parked
  // activation are deliberately distinct.
  void ArmWaitFork(CoroutineHandle waiter);

  // Parks `waiter` on this process's termination (LRM 9.7 `await`). It is
  // resumed once this process settles any terminal state -- normal completion
  // or a kill -- which is when the terminal transition drains the waiter list
  // into the runnable set. The caller checks the terminal state first, so a
  // process that is already terminated is never parked here.
  void ArmTerminatedWaiter(CoroutineHandle waiter);

  // Records that `leaf` is now blocked on `wait`: `leaf` becomes this process's
  // active leaf and takes the pending wait, set as one step so they never fall
  // out of step. Each suspending construct calls this from its `await_suspend`
  // after it enrolls. The active leaf is the one frame carrying the process's
  // thread -- the top frame before the body runs, the innermost parked frame
  // once a wait blocks it; process control (LRM 9.7) names the process and acts
  // on this leaf.
  void BlockLeaf(CoroutineHandle leaf, PendingWait* wait) {
    current_leaf_ = leaf;
    leaf->pending_wait = wait;
    // The vehicle carrying this thread when it blocks is the vehicle the
    // scheduler must drive to resume it. It is retained past the block (unlike
    // the registration), because resume runs from another process's context
    // where the ambient vehicle is that caller's, not this leaf's.
    resume_target_ = current_foreign_execution_;
  }
  [[nodiscard]] auto CurrentLeaf() const -> CoroutineHandle {
    return current_leaf_;
  }

  // Enters `fe` -- the foreign call the SV frame `continuation` is making --
  // and drives it to its first suspension or its return. `continuation` is the
  // frame to resume once the whole foreign call returns. Returns true if the
  // call returned without suspending (the caller continues inline), false if it
  // suspended across the boundary (an inner SV frame blocked and snapshotted
  // `fe` as its resume vehicle, so the scheduler resumes it later). The caller
  // owns `fe` and must keep it alive until the call returns.
  auto EnterForeignExecution(CoroutineHandle continuation, ForeignExecution& fe)
      -> bool;

  // The DPI current-scope chain (LRM 35.5.3). A `context` import brackets its
  // foreign call by pushing the scope of its declaration and popping on return;
  // `svGetScope` reads the top, `svSetScope` replaces it. The chain lives on
  // the process, not a thread-global, so two foreign calls suspended on
  // different processes never share one -- the invariant a shared thread-local
  // would break once time-consuming foreign calls interleave. It is non-empty
  // only inside a context import's foreign call.
  void PushDpiScope(Scope* scope) {
    dpi_scope_chain_.push_back(scope);
  }
  void PopDpiScope() {
    dpi_scope_chain_.pop_back();
  }
  [[nodiscard]] auto CurrentDpiScope() const -> Scope* {
    return dpi_scope_chain_.empty() ? nullptr : dpi_scope_chain_.back();
  }
  // `svSetScope` (LRM 35.5.3): retarget the current chain's scope, reporting
  // the previous one. Outside any context import the chain is empty and there
  // is no chain to retarget, so it is a no-op reporting null rather than
  // fabricating a frame with no lifetime boundary to pop it.
  auto ReplaceDpiScope(Scope* scope) -> Scope* {
    if (dpi_scope_chain_.empty()) {
      return nullptr;
    }
    Scope* previous = dpi_scope_chain_.back();
    dpi_scope_chain_.back() = scope;
    return previous;
  }

  // LRM 9.7 `suspend`: revoke the active leaf's scheduler participation -- a
  // detach, no scheduler verb -- and record the suspended state. The leaf's
  // pending wait is kept if it was blocked (resume re-establishes it) and
  // absent if it was runnable (resume re-queues); either way its registrations
  // are revoked. A process already suspended or terminated is unaffected.
  void Suspend();

  // LRM 9.7 `resume`: leave the suspended state onto the waiting axis so the
  // bridge that holds the engine can re-establish the leaf's pending wait or
  // re-queue it. This settles the state axis only; scheduling stays with the
  // bridge.
  void MarkResumed();

  // True when no immediate child is still executing: the whole live set has
  // reached a terminal state, or there were none. Terminated children retained
  // for their own live descendants (LRM 9.6.3) do not count as live. This is
  // the `wait fork` condition.
  [[nodiscard]] auto HasNoLiveChild() const -> bool;

  // If a frame is parked here on `wait fork` and the condition now holds,
  // unlink and return that frame for scheduling; null otherwise.
  [[nodiscard]] auto TakeWaitForkWaiterIfSatisfied() -> CoroutineHandle;

  // Bulk subtree termination (`kill` and `disable fork`) is one transactional
  // mutation: dismantle the whole subtree, collecting each node's local wake
  // effects into `woken`, while suppressing any intermediate parent-condition
  // re-evaluation; the caller then stabilizes the surviving lineage boundary
  // (the one parent whose child set shrank), re-evaluates its `wait fork`
  // there, and only then publishes `woken` to the scheduler. No node is
  // scheduled, and no parent predicate is republished, mid-dismantle -- so
  // nothing runs while the topology is half-mutated. Anything added to the
  // terminal transition must respect this: local effects collect here, boundary
  // effects at the caller, publication last.
  //
  // Forcibly terminates every descendant of this process -- not only its
  // immediate children, and including the descendants of subprocesses that have
  // already terminated (LRM 9.6.3 `disable fork`). This process itself keeps
  // running. Shares the kill primitive: each newly-terminated descendant is
  // marked KILLED and its frame released, so no queue, waiter list, or
  // subscription can name it, and every activation awaiting one is appended to
  // `woken` for the caller to schedule. A descendant kept alive by a `process`
  // handle survives as a parent-less terminal node reporting KILLED; the rest
  // are reclaimed.
  void DisableDescendants(std::vector<CoroutineHandle>& woken);

  // Forcibly terminates this process and its whole subtree (LRM 9.7 `kill`):
  // each node not already terminal is marked KILLED and its frame released, its
  // await waiters are appended to `woken`, and the lineage links inside the
  // subtree are severed so a handle-held node becomes a parent-less terminal
  // orphan and the rest are reclaimed. On return this node is severed from its
  // own children but still linked to its parent; the caller drops that link.
  void TerminateSubtreeKilled(std::vector<CoroutineHandle>& woken);

  // Terminates this subtree (LRM 9.7 `kill` / LRM 9.6 `disable`) when it
  // contains `running` -- the calling process, whose frame is executing and so
  // cannot be destroyed synchronously. Every off-path subtree is killed and
  // severed synchronously as usual; the single chain of nodes from this one
  // down to `running` is settled but kept linked, so `running` stays owned
  // until it unwinds to its safe boundary. `running` itself is only
  // termination-requested (phase 1), not settled: its terminal state is
  // published, and the whole retained chain released, when its body reaches the
  // resume boundary. The caller unwinds `running` after this returns.
  void TerminateSubtreeDeferringRunning(
      RuntimeProcess& running, std::vector<CoroutineHandle>& woken);

  // Whether `other` is this process or a descendant of it -- i.e. whether
  // terminating this subtree would tear down `other`'s frame. `kill` consults
  // it against the calling process to route it to the deferred self / ancestor
  // path.
  [[nodiscard]] auto IsSelfOrAncestorOf(const RuntimeProcess& other) const
      -> bool;

  // Phase 1 of a deferred, safe-boundary termination (LRM 9.6 / 9.7): the
  // calling process cannot destroy the frame it is running in, so termination
  // is split. This records the terminal cause and revokes the leaf's scheduler
  // participation -- nothing can wake it -- but does NOT publish the terminal
  // state or drain waiters, because the body is still going to run its unwind.
  // The terminal state, frame release, and waiter drain happen atomically later
  // (phase 2), when the body has unwound to a safe boundary.
  // Publishing nothing here is the point: an observer of `status()` or a
  // termination waiter never sees the process terminated while its body may
  // still run user code. Exactly-once -- a re-request on an already-requested
  // or terminated process is a no-op.
  void RequestTermination(ProcessTerminationCause cause);

  // Whether a deferred termination has been requested but not yet published --
  // the window between phase 1 and phase 2, during which the body unwinds.
  [[nodiscard]] auto TerminationRequested() const -> bool {
    return termination_requested_;
  }

  // Unlinks this process from its parent's lineage, dropping the parent's
  // ownership of it (a surviving `process` handle keeps the node alive as a
  // parent-less orphan). A process with no parent is owned by its scope and is
  // left in place. The caller must keep its own reference across the call, as
  // the parent's may have been the last.
  void DetachFromParent();

  // Releases `process`, whose body has just terminated, along with every
  // ancestor the release leaves with no lineage to retain, walking upward from
  // the leaf. Deliberately not a call on the released node: the release
  // destroys it. A process with no parent is owned by its scope and is never
  // released here.
  static void ReleaseTerminatedLineage(RuntimeProcess& process);

 private:
  // The guard is the sole writer of the ambient foreign-execution vehicle,
  // which it sets and restores around a foreign call.
  friend class ForeignExecutionGuard;

  // Drives `fe` for one stretch -- into its next suspension or its return --
  // with the foreign-execution context installed for its duration, so an inner
  // frame that blocks snapshots `fe` as its resume vehicle. Returns whether the
  // call has returned. Every drive goes through here so that installation can
  // never be forgotten.
  auto DriveForeignVehicle(ForeignExecution& fe) -> bool;

  // The one indivisible terminal transition, and the sole writer of the
  // terminal state: a body can never be marked terminated while it still owns
  // the frame it ran in, nor terminated without its own `await` waiters being
  // extracted. Sets terminal state + cause, releases the frame (which revokes
  // every registration it held), and drains this process's termination waiters
  // into `woken`. Split-off variants that settle without draining are
  // deliberately absent -- that separation was the footgun this primitive
  // removes.
  void SettleTerminated(
      ProcessTerminationCause cause, std::vector<CoroutineHandle>& woken);
  [[nodiscard]] auto IsReleasable() const -> bool;
  void EraseChild(RuntimeProcess& child);

  ProcessKind kind_;
  Scope* owning_scope_;
  Coroutine<void> coroutine_;
  // The frame the engine will resume next for this process (invariant: a
  // non-executing process has exactly one active leaf). Starts at the top frame
  // and follows the innermost parked frame as waits block it.
  CoroutineHandle current_leaf_ = nullptr;
  // The foreign execution this thread is running under right now, valid only
  // while a foreign call is on the stack; the guard sets and clears it. Null
  // outside any foreign call, which is every plain-coroutine process.
  ForeignExecution* current_foreign_execution_ = nullptr;
  // The vehicle the scheduler drives to resume the active leaf, snapshotted
  // from the ambient foreign execution when the leaf blocks and retained until
  // it blocks again. Null means resume the coroutine frame directly.
  ForeignExecution* resume_target_ = nullptr;
  // The SV frame to resume once the outermost foreign call returns -- the
  // import frame that entered it. An exported task suspends and completes
  // internally to the fiber, so its completion is not what continues the
  // process; this frame is. Null when no foreign call is outstanding.
  CoroutineHandle foreign_continuation_ = nullptr;
  // The DPI current-scope chain (LRM 35.5.3), one frame per active context
  // import in this process's foreign call chain. Empty outside any context
  // import.
  std::vector<Scope*> dpi_scope_chain_;
  ProcessExecutionState execution_state_ = ProcessExecutionState::kCreated;
  ProcessTerminationCause termination_cause_ =
      ProcessTerminationCause::kCompleted;
  // Set when a deferred termination is requested (phase 1) and consumed at the
  // resume boundary (phase 2): pending while the body unwinds.
  bool termination_requested_ = false;
  RuntimeProcess* parent_ = nullptr;
  std::vector<std::shared_ptr<RuntimeProcess>> children_;
  // The `wait fork` condition holds at most one activation: the frame that
  // executed `wait fork`.
  RegistrationList parked_wait_fork_;
  // The `await` condition (LRM 9.7): every activation waiting for this process
  // to terminate. Unlike `wait fork` it holds any number of waiters, drained
  // when this process settles a terminal state.
  RegistrationList terminated_waiters_;
};

}  // namespace lyra::runtime
