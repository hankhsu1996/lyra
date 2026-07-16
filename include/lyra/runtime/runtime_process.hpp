#pragma once

#include <cstdint>
#include <memory>
#include <vector>

#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/pending_wait.hpp"
#include "lyra/runtime/process_kind.hpp"
#include "lyra/runtime/registration.hpp"

namespace lyra::runtime {

class ExecutionContext;

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
class RuntimeProcess : public std::enable_shared_from_this<RuntimeProcess> {
 public:
  RuntimeProcess(ProcessKind kind, Coroutine<void> coroutine);

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
  // node outlives the frame. Taking the context is what makes this the only way
  // a body can run: the resumed body reaches its own process identity through
  // the context, and a caller cannot resume without supplying one.
  auto ResumeWith(
      ExecutionContext& context, CoroutineHandle handle,
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
  }
  [[nodiscard]] auto CurrentLeaf() const -> CoroutineHandle {
    return current_leaf_;
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

  // Whether `other` is this process or a descendant of it -- i.e. whether
  // terminating this subtree would tear down `other`'s frame. `kill` consults
  // it against the calling process to reject killing the running frame.
  [[nodiscard]] auto IsSelfOrAncestorOf(const RuntimeProcess& other) const
      -> bool;

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
  Coroutine<void> coroutine_;
  // The frame the engine will resume next for this process (invariant: a
  // non-executing process has exactly one active leaf). Starts at the top frame
  // and follows the innermost parked frame as waits block it.
  CoroutineHandle current_leaf_ = nullptr;
  ProcessExecutionState execution_state_ = ProcessExecutionState::kCreated;
  ProcessTerminationCause termination_cause_ =
      ProcessTerminationCause::kCompleted;
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
