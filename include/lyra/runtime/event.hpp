#pragma once

#include <vector>

#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/registration.hpp"

namespace lyra::runtime {

// The waiter set behind a named event (LRM 15.5): a trigger releases every
// activation parked on it at once, so unlike a value-change wait there is no
// per-membership fire condition to evaluate.
class RuntimeEvent {
 public:
  RuntimeEvent() = default;

  RuntimeEvent(const RuntimeEvent&) = delete;
  auto operator=(const RuntimeEvent&) -> RuntimeEvent& = delete;
  RuntimeEvent(RuntimeEvent&&) = delete;
  auto operator=(RuntimeEvent&&) -> RuntimeEvent& = delete;
  ~RuntimeEvent() = default;

  void AddWaiter(CoroutineHandle waiter) {
    waiter->Park(waiters_);
  }

  [[nodiscard]] auto TakeWaiters() -> std::vector<CoroutineHandle> {
    std::vector<CoroutineHandle> woken;
    while (Registration* reg = waiters_.PopFront()) {
      woken.push_back(reg->activation);
    }
    return woken;
  }

 private:
  RegistrationList waiters_;
};

}  // namespace lyra::runtime
