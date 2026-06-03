#pragma once

#include "lyra/base/time.hpp"
#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/runtime_services.hpp"

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
class DelayAwaitable {
 public:
  DelayAwaitable(
      RuntimeServices& services, SimDuration duration,
      std::int8_t precision_power)
      : services_(&services),
        duration_(duration),
        precision_power_(precision_power) {
  }

  [[nodiscard]] static auto await_ready() noexcept -> bool {
    return false;
  }

  void await_suspend(CoroutineHandle handle) noexcept {
    if (duration_ == 0) {
      services_->ScheduleInactive(handle);
    } else {
      const SimDuration global_ticks = ScaleToGlobalTicks(
          duration_, precision_power_, services_->GlobalPrecisionPower());
      services_->ScheduleAtTime(services_->Now() + global_ticks, handle);
    }
  }

  static void await_resume() noexcept {
  }

 private:
  RuntimeServices* services_;
  SimDuration duration_;
  std::int8_t precision_power_;
};

inline auto Delay(
    RuntimeServices& services, SimDuration duration,
    std::int8_t precision_power) -> DelayAwaitable {
  return DelayAwaitable{services, duration, precision_power};
}

}  // namespace lyra::runtime
