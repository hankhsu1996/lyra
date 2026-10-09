#include "lyra/runtime/process_control.hpp"

#include <cstdint>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/runtime/cancellation.hpp"
#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/object_ref.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/runtime/trigger.hpp"
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

// Waiting for another process to terminate (LRM 9.7 `process::await`). It is
// enrolled on the target's termination before the target's state is first
// asked, so a termination landing between the two is the one the asking sees.
class ProcessTerminationAwaiter final : public StateAwaiter {
 public:
  explicit ProcessTerminationAwaiter(value::ObjectRef target)
      : target_(std::move(target)) {
    EnrolOn(ProcessNodeOf(target_).Termination());
  }

  // Awaiting another process (LRM 9.7) is a method call, not an event control
  // or a wait statement, so LRM 12.4.2.1 does not make resuming from it a
  // violation report flush point.
  [[nodiscard]] auto IsReportFlushPoint() const -> bool override {
    return false;
  }

 private:
  [[nodiscard]] auto Holds() const -> bool override {
    return ProcessNodeOf(target_).ExecutionState() ==
           ProcessExecutionState::kTerminated;
  }

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
    target.TerminateSubtreeDeferringRunning(caller, runtime);
    RaiseUnclaimableEffect();
  }
  target.TerminateSubtreeKilled(runtime);
  // `self` still pins the target, so detaching it from the lineage cannot free
  // it mid-call. Killing the target may have satisfied a `wait fork` its parent
  // is parked on (LRM 9.6.1), and may leave a terminated ancestor with no live
  // descendant left to retain it.
  RuntimeProcess* parent = target.Parent();
  target.DetachFromParent();
  if (parent != nullptr) {
    runtime.WakeParkedOn(parent->WaitForkCondition(), Change::Whole());
    RuntimeProcess::ReleaseTerminatedLineage(*parent);
  }
}

auto ProcessAwait(const value::ObjectRef& self, RuntimeEffects& runtime)
    -> Wait {
  // The check is here at the call, symmetric with `suspend`, so the wait itself
  // is pure readiness.
  if (self.View<RuntimeProcess>() == &runtime.CurrentProcess()) {
    throw SimulationError(
        "process::await on the calling process is not allowed (LRM 9.7)");
  }
  return MakeWait<ProcessTerminationAwaiter>(self);
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
  ProcessNodeOf(self).Resume(runtime);
}

}  // namespace lyra::runtime
