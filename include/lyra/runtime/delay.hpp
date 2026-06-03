#pragma once

#include "lyra/base/time.hpp"
#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/runtime_services.hpp"

namespace lyra::runtime {

// Suspends the calling process for `duration` simulation-time units.
// `#0` enqueues on the inactive region of the current slot; `#N>0` enqueues
// at SimTime `now + N`. The engine does not know about delays as a category
// -- it only sees a process arriving in a queue at the right time.
class DelayAwaitable {
 public:
  DelayAwaitable(RuntimeServices& services, SimDuration duration)
      : services_(&services), duration_(duration) {
  }

  [[nodiscard]] static auto await_ready() noexcept -> bool {
    return false;
  }

  void await_suspend(CoroutineHandle handle) noexcept {
    if (duration_ == 0) {
      services_->ScheduleInactive(handle);
    } else {
      services_->ScheduleAtTime(services_->Now() + duration_, handle);
    }
  }

  static void await_resume() noexcept {
  }

 private:
  RuntimeServices* services_;
  SimDuration duration_;
};

inline auto Delay(RuntimeServices& services, SimDuration duration)
    -> DelayAwaitable {
  return DelayAwaitable{services, duration};
}

}  // namespace lyra::runtime
