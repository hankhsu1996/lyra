#pragma once

#include <utility>
#include <vector>

#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/observation.hpp"
#include "lyra/runtime/registration.hpp"

namespace lyra::runtime {

// The waiter set behind a named event (LRM 15.5). A trigger is the event
// itself, so nothing here has a value to compare; what a wait can still carry
// is an `iff` qualifier, which decides at the trigger and leaves the wait in
// place when it does not hold (LRM 9.4.2.3).
class RuntimeEvent {
 public:
  RuntimeEvent() = default;

  RuntimeEvent(const RuntimeEvent&) = delete;
  auto operator=(const RuntimeEvent&) -> RuntimeEvent& = delete;
  RuntimeEvent(RuntimeEvent&&) = delete;
  auto operator=(RuntimeEvent&&) -> RuntimeEvent& = delete;
  ~RuntimeEvent() = default;

  void AddWaiter(CoroutineHandle waiter, Observation observation = {}) {
    waiter->Park(waiters_).observation = std::move(observation);
  }

  // Claims and returns the activations this trigger is an event for; a wait
  // the qualifier holds back stays parked for the next one.
  [[nodiscard]] auto TakeFiringWaiters() -> std::vector<CoroutineHandle> {
    std::vector<CoroutineHandle> woken;
    waiters_.ForEach([&](Registration& reg) {
      if (!reg.FiresNow()) {
        return;
      }
      reg.Unlink();
      woken.push_back(reg.activation);
    });
    return woken;
  }

 private:
  RegistrationList waiters_;
};

}  // namespace lyra::runtime
