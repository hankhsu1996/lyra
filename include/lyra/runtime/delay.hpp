#pragma once

#include <cstdint>

#include "lyra/base/time.hpp"
#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/pending_wait.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::runtime {

// Scales `ticks`, expressed in `from_power` precision steps, up to the engine's
// `global_power` tick (LRM 3.14.3). `from_power >= global_power` because the
// global precision is the finest in the design, so the factor is a non-negative
// power of ten. A single-precision design has `from_power == global_power` and
// the factor is one.
inline auto ScaleToGlobalTicks(
    SimDuration ticks, std::int8_t from_power,
    std::int8_t global_power) noexcept -> SimDuration {
  SimDuration result = ticks;
  for (int i = 0; i < from_power - global_power; ++i) {
    result *= 10;
  }
  return result;
}

// Suspends the calling process for `duration` time steps of its scope's
// precision (`precision_power`). `#0` enqueues on the inactive region of the
// current slot; `#N>0` scales to the engine's global tick and enqueues at that
// future SimTime. The engine does not know about delays as a category -- it
// only sees a process arriving in a queue at the right time.
class DelayAwaitable : public PendingWait {
 public:
  DelayAwaitable(
      RuntimeEffects& runtime, SimDuration duration,
      std::int8_t precision_power)
      : runtime_(&runtime),
        duration_(duration),
        precision_power_(precision_power) {
  }

  [[nodiscard]] static auto await_ready() noexcept -> bool {
    return false;
  }

  template <class P>
  void await_suspend(std::coroutine_handle<P> handle) {
    CoroutineHandle token = &handle.promise();
    Arm(token);
    token->process->BlockLeaf(token, this);
  }

  static void await_resume() noexcept {
  }

  // A delay's deadline is absolute (LRM 9.7): on resume, if it has transpired
  // the process is runnable, otherwise it re-parks for the remaining time.
  auto Reestablish(RuntimeEffects& runtime, CoroutineHandle activation)
      -> PendingWaitOutcome override {
    if (runtime.Now() >= deadline_) {
      return PendingWaitOutcome::kRunnable;
    }
    runtime.ScheduleAtTime(deadline_, activation);
    return PendingWaitOutcome::kReblocked;
  }

 private:
  // Fixes the absolute deadline and parks: `#0` on the inactive region of this
  // slot (its deadline is this time, so a resume finds it transpired), `#N` at
  // the scaled future time.
  void Arm(CoroutineHandle token) {
    if (duration_ == 0) {
      deadline_ = runtime_->Now();
      runtime_->ScheduleInactive(token);
    } else {
      const SimDuration global_ticks = ScaleToGlobalTicks(
          duration_, precision_power_, runtime_->GlobalPrecisionPower());
      deadline_ = runtime_->Now() + global_ticks;
      runtime_->ScheduleAtTime(deadline_, token);
    }
  }

  RuntimeEffects* runtime_;
  SimDuration duration_;
  std::int8_t precision_power_;
  SimTime deadline_ = 0;
};

inline auto Delay(
    RuntimeEffects& runtime, const value::PackedArray& duration,
    const value::PackedArray& precision_power) -> DelayAwaitable {
  return DelayAwaitable{
      runtime, static_cast<SimDuration>(duration.ToInt64()),
      static_cast<std::int8_t>(precision_power.ToInt64())};
}

}  // namespace lyra::runtime
