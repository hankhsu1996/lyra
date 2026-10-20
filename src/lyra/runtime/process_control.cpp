#include "lyra/runtime/process_control.hpp"

#include <cstdint>
#include <optional>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/runtime/cancellation.hpp"
#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/object_ref.hpp"
#include "lyra/runtime/rng.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/runtime/wait.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/object_ref.hpp"
#include "lyra/value/string.hpp"

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
      : StateAwaiter(1), target_(std::move(target)) {
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

  // Holds the target across the suspension: once it has ended, whoever started
  // it releases it, and this wait still reads it after that.
  value::ObjectRef target_;
};

}  // namespace

auto ProcessSelf(RuntimeEffects& runtime) -> value::ObjectRef {
  RuntimeProcess* const running = runtime.TryCurrentProcess();
  if (running == nullptr) {
    return nullptr;
  }
  return RefToObject(running->shared_from_this());
}

auto ProcessStatus(const value::ObjectRef& self) -> value::Int {
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
  return value::Int::FromInt(static_cast<std::int32_t>(code));
}

void ProcessKill(const value::ObjectRef& self, RuntimeEffects& runtime) {
  RuntimeProcess& target = ProcessNodeOf(self);
  RuntimeProcess& caller = runtime.CurrentProcess();
  if (target.IsSelfOrAncestorOf(caller)) {
    target.TerminateSubtreeDeferringRunning(caller, runtime);
    RaiseUnclaimableEffect();
  }
  target.TerminateSubtreeKilled(runtime);
  // `self` still pins the target, so releasing it cannot free it mid-call. The
  // kill may leave a terminated ancestor with nothing under it to retain it.
  RuntimeProcess::ReleaseTerminatedLineage(target);
}

auto ProcessAwait(const value::ObjectRef& self, RuntimeEffects& runtime)
    -> Wait {
  // The check is here at the call, symmetric with `suspend`, so the wait itself
  // is pure readiness.
  if (self.View<RuntimeProcess>() == &runtime.CurrentProcess()) {
    // LRM 9.7.
    throw SimulationError(
        "'process::await' on the calling process is not allowed");
  }
  return MakeWait<ProcessTerminationAwaiter>(self);
}

void ProcessSuspend(const value::ObjectRef& self, RuntimeEffects& runtime) {
  // The activation layer does the state transition and the detach; nothing
  // here schedules.
  RuntimeProcess& target = ProcessNodeOf(self);
  if (&target == &runtime.CurrentProcess()) {
    // LRM 9.7.
    throw SimulationError(
        "'process::suspend' on the calling process is not allowed");
  }
  target.Suspend();
}

void ProcessResume(const value::ObjectRef& self, RuntimeEffects& runtime) {
  ProcessNodeOf(self).Resume(runtime);
}

void ProcessSrandom(
    const value::ObjectRef& self, const value::ConstIntegralView& seed) {
  ProcessNodeOf(self).Running().rng.Reseed(
      RandomSeed{static_cast<std::uint32_t>(value::ToInt64(seed))});
}

auto ProcessGetRandstate(const value::ObjectRef& self) -> value::String {
  return value::String{ProcessNodeOf(self).Running().rng.StateText()};
}

void ProcessSetRandstate(
    const value::ObjectRef& self, const value::String& state) {
  const std::optional<DrawRng> restored = DrawRng::FromStateText(state.View());
  if (!restored.has_value()) {
    throw SimulationError(
        "'process::set_randstate' was given a state that no 'get_randstate' "
        "returned");
  }
  ProcessNodeOf(self).Running().rng = *restored;
}

}  // namespace lyra::runtime
