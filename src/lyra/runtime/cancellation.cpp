#include "lyra/runtime/cancellation.hpp"

#include <cxxabi.h>
#include <exception>
#include <utility>

#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/runtime_process.hpp"

namespace lyra::runtime {

void CancellationTarget::Invalidate(RuntimeEffects& effects) {
  ++generation_;
  // Waking the blocked executions inside is the same act an event trigger
  // performs on what waits on it: a blocked execution would otherwise never
  // regain control -- a `wait` whose condition no longer becomes true is the
  // whole point -- and waking it takes it off the wait it was parked on, so
  // that wait cannot wake it a second time. An execution that is running,
  // already runnable, or stopped from outside is not blocked and reaches the
  // check on its own, when it next runs.
  members_.ForEach([&](TargetMembership& member) {
    if (Activation* blocked = member.process->BlockedLeaf()) {
      effects.Wake(blocked);
    }
  });
}

void EnterCancellationTarget(
    RuntimeEffects& effects, CancellationTarget* target) {
  effects.CurrentProcess().PushEnclosingTarget(target);
}

void LeaveCancellationTarget(
    RuntimeEffects& effects, CancellationTarget* target) {
  // A body's extent is left on every way out of it, and one of those ways runs
  // with nothing of the design executing: the frame of an execution still
  // suspended when the run ends is released where it stands, which runs the
  // cleanups it was holding open. There is no execution to withdraw the
  // membership from then, and none that could still consult it (LRM 9.6.2).
  if (RuntimeProcess* process = effects.TryCurrentProcess();
      process != nullptr) {
    process->PopEnclosingTarget(target);
  }
}

void TakeDepartureIfDue(RuntimeEffects& effects) {
  if (RuntimeProcess* process = effects.TryCurrentProcess();
      process != nullptr) {
    TakeDepartureIfDue(*process);
  }
}

void TakeDepartureIfDue(RuntimeProcess& process) {
  if (CancellationTarget* target = process.OutermostInvalidatedTarget();
      target != nullptr) {
    throw ControlEffect{.target = target};
  }
  if (process.DepartureIsDue()) {
    RaiseUnclaimableEffect();
  }
}

auto EffectNamesTarget(const ControlEffect& effect, CancellationTarget* target)
    -> bool {
  return effect.target == target;
}

void RaiseUnclaimableEffect() {
  throw ControlEffect{.target = nullptr};
}

void RaiseDeclinedDeparture(ControlEffect effect) {
  throw ControlEffect{.target = effect.target};
}

auto ClassifyUnwind() -> Unwound {
  // The raise is caught here in the same breath, so nothing leaves this
  // function still unwinding.
  try {
    throw;
  } catch (const ControlEffect&) {
    return {true, std::current_exception()};
  } catch (...) {
    return {false, std::current_exception()};
  }
}

void SettleRaised(RuntimeEffects& effects, const std::exception_ptr& raised) {
  ReportRaisedError(effects, raised);
  if (RuntimeProcess* process = effects.TryCurrentProcess();
      process != nullptr) {
    process->RequestTermination(ProcessTerminationCause::kKilled);
  }
}

auto ReceiveDeparture() -> ControlEffect {
  // Asked the same way as above, and for the same reason. Anything neither
  // arm names leaves by the same raise that asked, which carries it on
  // unchanged.
  try {
    throw;
  } catch (const ControlEffect& effect) {
    return effect;
  } catch (const std::exception&) {
    SettleRaised(current_runtime(), std::current_exception());
    return ControlEffect{.target = nullptr};
  }
}

namespace {

// Ends the catch a landing began when the receiving is left by a raise, as the
// cleanup a C++ handler carries ends it on that way out; a return leaves it to
// the landing's own exit. What is in flight is counted once the catch has
// begun, since the exception it began was in flight until then.
class EndCatchOnRaise {
 public:
  explicit EndCatchOnRaise(void* exception)
      : in_flight_(BeginCatch(exception)) {
  }
  EndCatchOnRaise(const EndCatchOnRaise&) = delete;
  auto operator=(const EndCatchOnRaise&) -> EndCatchOnRaise& = delete;
  EndCatchOnRaise(EndCatchOnRaise&&) = delete;
  auto operator=(EndCatchOnRaise&&) -> EndCatchOnRaise& = delete;
  ~EndCatchOnRaise() {
    if (std::uncaught_exceptions() > in_flight_) {
      abi::__cxa_end_catch();
    }
  }

 private:
  static auto BeginCatch(void* exception) -> int {
    abi::__cxa_begin_catch(exception);
    return std::uncaught_exceptions();
  }

  int in_flight_;
};

}  // namespace

auto ReceiveDepartureOf(void* exception) -> ControlEffect {
  const EndCatchOnRaise handling(exception);
  return ReceiveDeparture();
}

Unwound::Unwound(bool control_effect, std::exception_ptr raised)
    : control_effect(control_effect), raised(std::move(raised)) {
}
Unwound::Unwound(const Unwound&) = default;
auto Unwound::operator=(const Unwound&) -> Unwound& = default;
Unwound::Unwound(Unwound&&) noexcept = default;
auto Unwound::operator=(Unwound&&) noexcept -> Unwound& = default;
Unwound::~Unwound() = default;

CancellationTarget::CancellationTarget() = default;
CancellationTarget::~CancellationTarget() = default;

void Disable(CancellationTarget* target, RuntimeEffects& effects) {
  target->Invalidate(effects);
  TakeDepartureIfDue(effects.CurrentProcess());
}

}  // namespace lyra::runtime
