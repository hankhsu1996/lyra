#include "lyra/runtime/cancellation.hpp"

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
    effects.ScheduleNextDelta(waiter->activation);
  }
}

void EnterCancellationTarget(
    RuntimeEffects& effects, CancellationTarget* target) {
  effects.CurrentProcess().PushEnclosingTarget(target);
}

void LeaveCancellationTarget(
    RuntimeEffects& effects, CancellationTarget* target) {
  effects.CurrentProcess().PopEnclosingTarget(target);
}

void RaiseControlEffectIfDisabled(RuntimeProcess& process) {
  CancellationTarget* target = process.OutermostInvalidatedTarget();
  if (target == nullptr) {
    return;
  }
  throw ControlEffect{.target = target};
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
    return Unwound{.control_effect = true, .raised = std::current_exception()};
  } catch (...) {
    return Unwound{.control_effect = false, .raised = std::current_exception()};
  }
}

void Disable(CancellationTarget* target, RuntimeEffects& effects) {
  target->Invalidate(effects);
  RaiseControlEffectIfDisabled(effects.CurrentProcess());
}

}  // namespace lyra::runtime
