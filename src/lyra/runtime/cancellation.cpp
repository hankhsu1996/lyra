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

void RaiseAbortIfDisabled(RuntimeProcess& process) {
  CancellationSource* target = process.OutermostInvalidatedTarget();
  if (target == nullptr) {
    return;
  }
  // The process records that it is leaving a disabled target before the effect
  // is raised, so a frame that ends up settling this execution reads why it
  // ended without having to inspect what is unwinding through it.
  process.NoteAbortRaised();
  throw Abort{.target = target, .leaving = &process};
}

void AbortConsumeOrRethrow(const Abort& abort, CancellationSource& target) {
  if (abort.target != &target) {
    throw abort;
  }
  // The execution stops leaving: it resumes past the target it just left, and
  // whatever outcome it later settles is its own, not this termination.
  abort.leaving->NoteAbortConsumed();
}

void Disable(CancellationSource& target, RuntimeEffects& effects) {
  target.Invalidate(effects);
  RaiseAbortIfDisabled(effects.CurrentProcess());
}

}  // namespace lyra::runtime
