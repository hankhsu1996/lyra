#include "lyra/runtime/cancellation.hpp"

#include <exception>
#include <functional>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/runtime_process.hpp"

namespace lyra::runtime {

void CancellationTarget::Invalidate(RuntimeEffects& effects) {
  ++generation_;
  // Releasing the waiters is the same act an event trigger performs on its own:
  // a blocked execution would otherwise never regain control -- a `wait` whose
  // condition no longer becomes true is the whole point -- and waking it
  // revokes the registrations it holds elsewhere, so its wait settles exactly
  // once. An execution that is running or already runnable is not waiting on
  // this target and reaches the check on its own.
  while (Registration* waiter = cancel_waiters_.PopFront()) {
    effects.Wake(waiter->activation);
  }
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

void RaiseControlEffect(CancellationTarget* target) {
  throw ControlEffect{.target = target};
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

auto ClaimableTarget() -> CancellationTarget* {
  // Asked the same way as above, and for the same reason. Anything else leaves
  // by the same raise that asked, which carries it on unchanged.
  try {
    throw;
  } catch (const ControlEffect& effect) {
    return effect.target;
  }
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

void RunAsLanding(
    RuntimeEffects& effects, const std::function<void()>& stretch) {
  try {
    stretch();
  } catch (const ControlEffect& effect) {
    if (effect.target != nullptr) {
      throw InternalError(
          "RunAsLanding: a departure a region owns left every region");
    }
  } catch (const std::exception&) {
    ReportRaisedError(effects, std::current_exception());
  }
}

void Disable(CancellationTarget* target, RuntimeEffects& effects) {
  target->Invalidate(effects);
  TakeDepartureIfDue(effects.CurrentProcess());
}

}  // namespace lyra::runtime
