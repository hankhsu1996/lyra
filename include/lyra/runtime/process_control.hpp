#pragma once

#include <cstdint>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/runtime/cancellation.hpp"
#include "lyra/runtime/object_ref.hpp"
#include "lyra/runtime/registration.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/runtime/wait.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::runtime {

// LRM 9.7 `process::state`, in declaration order: `status()` reports the
// process's state as one of these, and the underlying integer is what the
// SystemVerilog program compares against the enum members. The simulated
// program is what tells these apart, never Lyra, so it is not a dispatch set.
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
inline auto ProcessSelf(RuntimeEffects& runtime) -> value::ObjectRef {
  return RefToObject(runtime.CurrentProcess().shared_from_this());
}

// The process node a reference names. A reference states which object it refers
// to and leaves what may be read through it to the program point holding it;
// every point here assumes the same class, so it is named once.
inline auto ProcessNodeOf(const value::ObjectRef& self) -> RuntimeProcess& {
  return self.Deref<RuntimeProcess>();
}

// LRM 9.7 `process::status()`: the process's execution state projected onto the
// LRM `state` enum. The state lives on the persistent process node rather than
// the coroutine frame, so a process remains observable through a surviving
// handle after its body terminates. A terminated process reports FINISHED or
// KILLED by how it terminated -- the completion slot is gone by then, so the
// distinction is read from the node's persistent terminal cause.
inline auto ProcessStatus(const value::ObjectRef& self)
    -> lyra::value::PackedArray {
  const RuntimeProcess& node = ProcessNodeOf(self);
  const ProcessStatusCode code = [&] {
    switch (node.ExecutionState()) {
      case ProcessExecutionState::kCreated:
      case ProcessExecutionState::kRunning:
        return ProcessStatusCode::kRunning;
      case ProcessExecutionState::kWaiting:
        return ProcessStatusCode::kWaiting;
      case ProcessExecutionState::kSuspended:
        return ProcessStatusCode::kSuspended;
      case ProcessExecutionState::kTerminated:
        return node.TerminationCause() == ProcessTerminationCause::kKilled
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
inline void ProcessKill(const value::ObjectRef& self, RuntimeEffects& runtime) {
  RuntimeProcess& target = ProcessNodeOf(self);
  RuntimeProcess& caller = runtime.CurrentProcess();
  if (target.IsSelfOrAncestorOf(caller)) {
    std::vector<CoroutineHandle> woken;
    target.TerminateSubtreeDeferringRunning(caller, woken);
    for (CoroutineHandle waiter : woken) {
      runtime.Wake(waiter);
    }
    RaiseUnclaimableEffect();
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
    runtime.Wake(waiter);
  }
}

// Waiting for another process to terminate, normally or forcibly (LRM 9.7
// `process::await`). Termination is monotonic -- the target terminates once --
// so waiting again after the caller was stopped is the same question asked
// afresh, and a target that has already terminated leaves nothing to wait for.
//
// Correctness rests on the single-engine serialization Lyra's scheduler
// provides: observing the target non-terminal and arming the waiter run in the
// same stretch of execution with no engine re-entry between them, so the target
// cannot terminate in that gap. A parallel or re-entrant engine would need an
// atomic arm-or-observe protocol here.
class ProcessTerminationWait : public Wait {
 public:
  explicit ProcessTerminationWait(value::ObjectRef target)
      : target_(std::move(target)) {
  }

  // NOLINTNEXTLINE(readability-named-parameter)
  auto Begin(RuntimeEffects&, CoroutineHandle leaf) -> WaitOutcome override {
    if (ProcessNodeOf(target_).ExecutionState() ==
        ProcessExecutionState::kTerminated) {
      return WaitOutcome::kSatisfied;
    }
    ProcessNodeOf(target_).ArmTerminatedWaiter(leaf);
    return WaitOutcome::kBlocked;
  }

  // Awaiting another process (LRM 9.7) is a method call, not an event control
  // or a wait statement, so LRM 12.4.2.1 does not make resuming from it a
  // violation report flush point.
  [[nodiscard]] auto IsReportFlushPoint() const -> bool override {
    return false;
  }

 private:
  // Pins the target across the suspension, so a kill that detaches it from the
  // lineage while the caller is parked cannot free the node before resume.
  value::ObjectRef target_;
};

// It is an error to await the calling process (a process cannot wait for its
// own termination). The check is here at the call, symmetric with `suspend`, so
// the wait itself is pure readiness.
inline auto ProcessAwait(const value::ObjectRef& self, RuntimeEffects& runtime)
    -> bool {
  if (self.View<RuntimeProcess>() == &runtime.CurrentProcess()) {
    throw SimulationError(
        "process::await on the calling process is not allowed (LRM 9.7)");
  }
  return runtime.CurrentProcess().ParkOn<ProcessTerminationWait>(runtime, self);
}

// LRM 9.7 `process::suspend()`: pause a process. It is an error to suspend the
// calling process (a function cannot suspend its own execution). Suspending a
// process that is already suspended or terminated has no effect. The activation
// layer does the state transition and the detach; nothing here schedules.
inline void ProcessSuspend(
    const value::ObjectRef& self, RuntimeEffects& runtime) {
  RuntimeProcess& target = ProcessNodeOf(self);
  if (&target == &runtime.CurrentProcess()) {
    throw SimulationError(
        "process::suspend on the calling process is not allowed (LRM 9.7)");
  }
  target.Suspend();
}

// LRM 9.7 `process::resume()`: restart a suspended process. A process that is
// not suspended is unaffected. Otherwise it waits again for the same thing it
// was waiting for, and runs in the current time step where that has already
// happened -- which is the whole of the clause, because a process that was
// runnable when it was stopped is one whose wait was already satisfied.
inline void ProcessResume(
    const value::ObjectRef& self, RuntimeEffects& runtime) {
  RuntimeProcess& target = ProcessNodeOf(self);
  if (target.ExecutionState() != ProcessExecutionState::kSuspended) {
    return;
  }
  const CoroutineHandle leaf = target.CurrentLeaf();
  target.MarkResumed();
  // An activation holds what it is waiting for exactly while it is blocked, so
  // holding nothing is how a process stopped while already runnable -- woken,
  // but not yet run -- says that it has nothing left to wait for.
  const bool satisfied =
      leaf->wait == nullptr ||
      leaf->wait->Again(runtime, leaf) == WaitOutcome::kSatisfied;
  if (satisfied) {
    runtime.Wake(leaf);
  }
}

}  // namespace lyra::runtime
