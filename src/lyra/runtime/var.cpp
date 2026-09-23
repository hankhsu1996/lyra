#include "lyra/runtime/var.hpp"

#include <span>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/observation.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/runtime/trigger.hpp"
#include "lyra/runtime/wait.hpp"

namespace lyra::runtime {

namespace {

// Waiting for one of an event control's leaves to report an occurrence. None of
// what a leaf reports is a level: what happens while the procedure is not
// waiting here is missed, so waiting again waits for the next occurrence and
// compares against what it finds now rather than against what it left, which is
// what LRM 9.7 asks of an event expression when a stopped process is started
// again.
class EventControlWait : public Wait {
 public:
  explicit EventControlWait(std::span<const Trigger> triggers)
      : triggers_(triggers.begin(), triggers.end()) {
  }

  // NOLINTNEXTLINE(readability-named-parameter)
  auto Begin(RuntimeEffects&, CoroutineHandle leaf) -> WaitOutcome override {
    SubscribeToLeaves(leaf, triggers_);
    return WaitOutcome::kBlocked;
  }

  auto Again(RuntimeEffects& services, CoroutineHandle leaf)
      -> WaitOutcome override {
    // The triggers were armed against the values as they stood when the wait
    // was first made, and those are not the values a change is now measured
    // from: whatever moved while the process was stopped is not an occurrence
    // this wait may report.
    for (const Trigger& trigger : triggers_) {
      if (ArmedObservation* observation = trigger.observation.Get()) {
        observation->Arm();
      }
    }
    return Begin(services, leaf);
  }

  // The construct behind this wait is an event control, a `wait` condition, or
  // an always_comb / always_latch sensitivity list -- each of which
  // LRM 12.4.2.1 names as a violation report flush point when it resumes the
  // process.
  [[nodiscard]] auto IsReportFlushPoint() const -> bool override {
    return true;
  }

 private:
  std::vector<Trigger> triggers_;
};

// Waiting for a condition the body itself tests (LRM 9.4.3). It watches the
// same leaves, because a change to what the condition reads is the only thing
// that can make it true -- but what it waits for is a state rather than an
// occurrence, and the only thing able to read that state is the body. So
// starting again after the process was stopped answers satisfied and lets the
// body's own loop decide, which is exactly the evaluation LRM 9.7 calls for:
// "resensitize the process ... to wait for the wait condition to become true.
// If the wait condition is now true ... the process is scheduled into the
// Active or Reactive region to continue its execution in the current time
// step."
class LevelConditionWait : public EventControlWait {
 public:
  using EventControlWait::EventControlWait;

  // NOLINTNEXTLINE(readability-named-parameter)
  auto Again(RuntimeEffects&, CoroutineHandle) -> WaitOutcome override {
    return WaitOutcome::kSatisfied;
  }
};

}  // namespace

void SubscribeToLeaves(
    CoroutineHandle frame, std::span<const Trigger> triggers) {
  for (const Trigger& trigger : triggers) {
    if (trigger.observable == nullptr) {
      throw InternalError("SubscribeToLeaves: a leaf names nothing to wait on");
    }
    trigger.observable->Subscribe(
        frame, trigger.observation, trigger.lsb_bit_offset, trigger.bit_width);
  }
}

auto WaitAny(RuntimeEffects& services, std::span<const Trigger> triggers)
    -> bool {
  return services.CurrentProcess().ParkOn<EventControlWait>(services, triggers);
}

auto WaitUntil(RuntimeEffects& services, std::span<const Trigger> triggers)
    -> bool {
  return services.CurrentProcess().ParkOn<LevelConditionWait>(
      services, triggers);
}

}  // namespace lyra::runtime
