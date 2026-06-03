#pragma once

#include <vector>

#include "lyra/runtime/coroutine.hpp"

namespace lyra::runtime {

class RuntimeEvent {
 public:
  RuntimeEvent() = default;

  RuntimeEvent(const RuntimeEvent&) = delete;
  auto operator=(const RuntimeEvent&) -> RuntimeEvent& = delete;
  RuntimeEvent(RuntimeEvent&&) = delete;
  auto operator=(RuntimeEvent&&) -> RuntimeEvent& = delete;
  ~RuntimeEvent() = default;

  void AddWaiter(CoroutineHandle waiter);
  [[nodiscard]] auto TakeWaiters() -> std::vector<CoroutineHandle>;

 private:
  std::vector<CoroutineHandle> waiters_;
};

}  // namespace lyra::runtime
