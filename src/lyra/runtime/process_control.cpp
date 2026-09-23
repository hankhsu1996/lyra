#include "lyra/runtime/process_control.hpp"

#include <cstdint>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/runtime/cancellation.hpp"
#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/object_ref.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/runtime/wait.hpp"
#include "lyra/value/object_ref.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::runtime {

namespace {

// The process node a reference names. A reference states which object it refers
// to and leaves what may be read through it to the program point holding it;
// every point here assumes the same class, so it is named once.
auto ProcessNodeOf(const value::ObjectRef& self) -> RuntimeProcess& {
  return self.Deref<RuntimeProcess>();
}

// Waiting for another process to terminate (LRM 9.7 `process::await`).
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

}  // namespace

auto ProcessSelf(RuntimeEffects& runtime) -> value::ObjectRef {
  return RefToObject(runtime.CurrentProcess().shared_from_this());
}

auto ProcessStatus(const value::ObjectRef& self) -> lyra::value::PackedArray {
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

void ProcessKill(const value::ObjectRef& self, RuntimeEffects& runtime) {
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

auto ProcessAwait(const value::ObjectRef& self, RuntimeEffects& runtime)
    -> bool {
  // The check is here at the call, symmetric with `suspend`, so the wait itself
  // is pure readiness.
  if (self.View<RuntimeProcess>() == &runtime.CurrentProcess()) {
    throw SimulationError(
        "process::await on the calling process is not allowed (LRM 9.7)");
  }
  return runtime.CurrentProcess().ParkOn<ProcessTerminationWait>(runtime, self);
}

void ProcessSuspend(const value::ObjectRef& self, RuntimeEffects& runtime) {
  // The activation layer does the state transition and the detach; nothing
  // here schedules.
  RuntimeProcess& target = ProcessNodeOf(self);
  if (&target == &runtime.CurrentProcess()) {
    throw SimulationError(
        "process::suspend on the calling process is not allowed (LRM 9.7)");
  }
  target.Suspend();
}

void ProcessResume(const value::ObjectRef& self, RuntimeEffects& runtime) {
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
