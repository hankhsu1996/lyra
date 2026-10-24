#include "lyra/runtime/cancellation.hpp"

#include <exception>
#include <memory>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/support/target_landing.hpp"

namespace lyra::runtime {

void CancellationTarget::Invalidate(RuntimeEffects& effects) {
  ++generation_;
  // Waking the blocked executions that land past the target is the same act an
  // event trigger performs on what waits on it: a blocked execution would
  // otherwise never regain control -- a `wait` whose condition no longer
  // becomes true is the whole point -- and waking it takes it off the wait it
  // was parked on, so that wait cannot wake it a second time. An execution that
  // is running, already runnable, or stopped from outside is not blocked and
  // reaches the check on its own, when it next runs.
  //
  // An execution that lands at its own end has nothing to regain control for,
  // so it is ended here. Ending one releases its frame, which leaves every
  // target it is inside and may end its descendants, so the ones to end are
  // gathered first and each is held until it has been ended.
  std::vector<std::shared_ptr<RuntimeProcess>> ended;
  members_.ForEach([&](TargetMembership& member) {
    switch (member.landing) {
      case support::TargetLanding::kPastTheTarget:
        if (Activation* blocked = member.process->BlockedLeaf()) {
          effects.Wake(blocked);
        }
        return;
      case support::TargetLanding::kAtExecutionEnd:
        ended.push_back(member.process->shared_from_this());
        return;
    }
    throw InternalError("CancellationTarget::Invalidate: unknown landing");
  });
  for (const std::shared_ptr<RuntimeProcess>& process : ended) {
    RuntimeProcess::Kill(*process, effects);
  }
}

void EnterTargetLandingPastIt(
    RuntimeEffects& effects, CancellationTarget* target) {
  effects.CurrentProcess().PushEnclosingTarget(
      target, support::TargetLanding::kPastTheTarget);
}

void EnterTargetLandingAtExecutionEnd(
    RuntimeEffects& effects, CancellationTarget* target) {
  effects.CurrentProcess().PushEnclosingTarget(
      target, support::TargetLanding::kAtExecutionEnd);
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
  if (process.TerminationIsDue()) {
    RaiseUnclaimableEffect();
  }
  if (CancellationTarget* target = process.OutermostInvalidatedTarget();
      target != nullptr) {
    throw ControlEffect{.target = target};
  }
}

auto RegionLandsEffect(
    RuntimeEffects& effects, const ControlEffect& effect,
    CancellationTarget* target) -> bool {
  if (effect.target != target) {
    return false;
  }
  const RuntimeProcess* process = effects.TryCurrentProcess();
  return process == nullptr || !process->DepartureIsDue();
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
