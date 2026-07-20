#pragma once

#include <coroutine>
#include <cstdint>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/gc_ref.hpp"
#include "lyra/runtime/pending_wait.hpp"
#include "lyra/runtime/registration.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/runtime/runtime_services.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::runtime {

// LRM 9.7 `process::state`, in declaration order: `status()` reports the
// process's state as one of these, and the underlying integer is what the
// SystemVerilog program compares against the enum members.
enum class ProcessStatusCode : std::int32_t {
  kFinished = 0,
  kRunning = 1,
  kWaiting = 2,
  kSuspended = 3,
  kKilled = 4,
};

// LRM 9.7 `process::self()`: a handle to the process making the call. A task or
// function runs in its caller's thread (LRM 9.5), so this returns the enclosing
// executing process, reached through the ambient execution context.
inline auto ProcessSelf(RuntimeServices& services) -> GcRef<RuntimeProcess> {
  return GcRef<RuntimeProcess>(services.CurrentProcess().shared_from_this());
}

// LRM 9.7 `process::status()`: the process's execution state projected onto the
// LRM `state` enum. The state lives on the persistent process node rather than
// the coroutine frame, so a process remains observable through a surviving
// handle after its body terminates. A terminated process reports FINISHED or
// KILLED by how it terminated -- the completion slot is gone by then, so the
// distinction is read from the node's persistent terminal cause.
inline auto ProcessStatus(const GcRef<RuntimeProcess>& self)
    -> lyra::value::PackedArray {
  const ProcessStatusCode code = [&] {
    switch (self->ExecutionState()) {
      case ProcessExecutionState::kCreated:
      case ProcessExecutionState::kRunning:
        return ProcessStatusCode::kRunning;
      case ProcessExecutionState::kWaiting:
        return ProcessStatusCode::kWaiting;
      case ProcessExecutionState::kSuspended:
        return ProcessStatusCode::kSuspended;
      case ProcessExecutionState::kTerminated:
        return self->TerminationCause() == ProcessTerminationCause::kKilled
                   ? ProcessStatusCode::kKilled
                   : ProcessStatusCode::kFinished;
    }
    throw InternalError("ProcessStatus: unknown process execution state");
  }();
  return lyra::value::PackedArray::Int(static_cast<std::int32_t>(code));
}

// LRM 9.7 `process::kill()`: forcibly terminate the process and all its
// descendant subprocesses. Each terminated node is marked KILLED and its frame
// released, so nothing can resume it, and every process awaiting one is woken.
//
// Killing the calling process or one of its ancestors is a deferred,
// safe-boundary termination: a running coroutine cannot destroy the frame it is
// executing in, and that frame is somewhere in the killed subtree. Every
// off-path node (each parked at a safe boundary) is torn down synchronously,
// while the chain that owns the running frame is kept linked so it stays alive;
// the running process's own termination is requested (registrations revoked,
// cause recorded) and its body is unwound to the engine's resume boundary,
// where the terminal state is published and the retained chain released.
inline void ProcessKill(
    const GcRef<RuntimeProcess>& self, RuntimeServices& services) {
  RuntimeProcess& target = *self;
  RuntimeProcess& caller = services.CurrentProcess();
  if (target.IsSelfOrAncestorOf(caller)) {
    std::vector<CoroutineHandle> woken;
    target.TerminateSubtreeDeferringRunning(caller, woken);
    for (CoroutineHandle waiter : woken) {
      services.ScheduleNextDelta(waiter);
    }
    UnwindForProcessTermination();
  }
  std::vector<CoroutineHandle> woken;
  target.TerminateSubtreeKilled(woken);
  // `self` still pins the target, so detaching it from the lineage cannot free
  // it mid-call. Killing the target may have satisfied a `wait fork` its parent
  // is parked on (LRM 9.6.1), and may leave a terminated ancestor with no live
  // descendant left to retain it.
  RuntimeProcess* parent = target.Parent();
  target.DetachFromParent();
  if (parent != nullptr) {
    if (CoroutineHandle waiter = parent->TakeWaitForkWaiterIfSatisfied()) {
      woken.push_back(waiter);
    }
    RuntimeProcess::ReleaseTerminatedLineage(*parent);
  }
  for (CoroutineHandle waiter : woken) {
    services.ScheduleNextDelta(waiter);
  }
}

// LRM 9.7 `process::await()`: suspend the caller until `self` terminates,
// normally or forcibly. Parks on the target's termination and resumes when it
// settles; a target that has already terminated does not suspend at all.
//
// Correctness rests on the single-engine serialization Lyra's scheduler
// provides: `await_ready` observing the target non-terminal and `await_suspend`
// arming the waiter run in the same coroutine resume with no engine re-entry
// between them, so the target cannot terminate in that gap. A parallel or
// re-entrant engine would need an atomic arm-or-observe protocol here.
class ProcessAwaitAwaitable : public PendingWait {
 public:
  explicit ProcessAwaitAwaitable(GcRef<RuntimeProcess> target)
      : target_(std::move(target)) {
  }

  [[nodiscard]] auto await_ready() const -> bool {
    return target_->ExecutionState() == ProcessExecutionState::kTerminated;
  }

  template <class P>
  void await_suspend(std::coroutine_handle<P> waiter) {
    CoroutineHandle token = &waiter.promise();
    target_->ArmTerminatedWaiter(token);
    token->process->BlockLeaf(token, this);
  }

  void await_resume() const noexcept {
  }

  // Termination is monotonic (LRM 9.7): the target terminates once. On resume,
  // if it has terminated the awaiter is runnable; otherwise re-park on its
  // termination. No engine services are needed.
  // NOLINTNEXTLINE(readability-named-parameter)
  auto Reestablish(RuntimeServices&, CoroutineHandle activation)
      -> PendingWaitOutcome override {
    if (target_->ExecutionState() == ProcessExecutionState::kTerminated) {
      return PendingWaitOutcome::kRunnable;
    }
    target_->ArmTerminatedWaiter(activation);
    return PendingWaitOutcome::kReblocked;
  }

 private:
  // Pins the target across the suspension, so a kill that detaches it from the
  // lineage while the caller is parked cannot free the node before resume.
  GcRef<RuntimeProcess> target_;
};

// It is an error to await the calling process (a process cannot wait for its
// own termination). The check is here at the call, symmetric with `suspend`, so
// the awaitable itself is pure readiness.
inline auto ProcessAwait(
    const GcRef<RuntimeProcess>& self, RuntimeServices& services)
    -> ProcessAwaitAwaitable {
  if (self.Get() == &services.CurrentProcess()) {
    throw InternalError(
        "process::await on the calling process is not allowed (LRM 9.7)");
  }
  return ProcessAwaitAwaitable{self};
}

// LRM 9.7 `process::suspend()`: pause a process. It is an error to suspend the
// calling process (a function cannot suspend its own execution). Suspending a
// process that is already suspended or terminated has no effect. The activation
// layer does the state transition and the detach; nothing here schedules.
inline void ProcessSuspend(
    const GcRef<RuntimeProcess>& self, RuntimeServices& services) {
  RuntimeProcess& target = *self;
  if (&target == &services.CurrentProcess()) {
    throw InternalError(
        "process::suspend on the calling process is not allowed (LRM 9.7)");
  }
  target.Suspend();
}

// LRM 9.7 `process::resume()`: restart a suspended process. A process that is
// not suspended is unaffected. A process suspended while blocked re-establishes
// its wait through the leaf's pending wait -- re-enrolling, or becoming
// runnable if the condition is already satisfied; a process suspended while
// runnable is re-queued to run in the current time step.
inline void ProcessResume(
    const GcRef<RuntimeProcess>& self, RuntimeServices& services) {
  RuntimeProcess& target = *self;
  if (target.ExecutionState() != ProcessExecutionState::kSuspended) {
    return;
  }
  const CoroutineHandle leaf = target.CurrentLeaf();
  target.MarkResumed();
  // A leaf suspended while runnable has no pending wait and is simply
  // re-queued; one suspended while blocked re-establishes its wait, which
  // either re-enrolls (nothing to schedule) or reports the condition already
  // satisfied.
  PendingWait* pending = leaf->pending_wait;
  const bool runnable =
      pending == nullptr ||
      pending->Reestablish(services, leaf) == PendingWaitOutcome::kRunnable;
  if (runnable) {
    services.ScheduleNextDelta(leaf);
  }
}

}  // namespace lyra::runtime
