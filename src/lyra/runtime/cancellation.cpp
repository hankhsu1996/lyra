#include "lyra/runtime/cancellation.hpp"

#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/runtime_process.hpp"

namespace lyra::runtime {

void CancellationSource::Invalidate(RuntimeEffects& effects) {
  ++generation_;
  // Releasing the waiters is the same act an event trigger performs on its own:
  // a blocked execution would otherwise never regain control -- a `wait` whose
  // condition no longer becomes true is the whole point -- and waking it
  // revokes the registrations it holds elsewhere, so its wait settles exactly
  // once. An execution that is running or already runnable is not waiting on
  // this target and reaches the check on its own.
  while (Registration* waiter = cancel_waiters_.PopFront()) {
    effects.ScheduleNextDelta(waiter->activation);
  }
}

CancellationGuard::CancellationGuard(
    RuntimeEffects& effects, CancellationSource* source)
    : process_(&effects.CurrentProcess()), source_(source) {
  process_->PushEnclosingTarget(source);
}

CancellationGuard::~CancellationGuard() {
  process_->PopEnclosingTarget(source_);
}

void RaiseControlEffectIfDisabled(RuntimeProcess& process) {
  CancellationSource* target = process.OutermostInvalidatedTarget();
  if (target == nullptr) {
    return;
  }
  throw ControlEffect{.target = target};
}

void ClaimControlEffect(
    const ControlEffect& effect, CancellationSource& target) {
  // Returning is what claiming the effect means: the execution resumes past the
  // target it just left, and whatever outcome it later settles is its own, not
  // this one.
  if (effect.target != &target) {
    throw ControlEffect{effect};
  }
}

void RaiseUnclaimableEffect() {
  throw ControlEffect{.target = nullptr};
}

auto ClassifyUnwind() -> Unwound {
  // The raise is caught here in the same breath, so nothing leaves this
  // function still unwinding.
  try {
    throw;
  } catch (const ControlEffect&) {
    return Unwound{.control_effect = true, .raised = std::current_exception()};
  } catch (...) {
    return Unwound{.control_effect = false, .raised = std::current_exception()};
  }
}

void Disable(CancellationSource& target, RuntimeEffects& effects) {
  target.Invalidate(effects);
  RaiseControlEffectIfDisabled(effects.CurrentProcess());
}

}  // namespace lyra::runtime
