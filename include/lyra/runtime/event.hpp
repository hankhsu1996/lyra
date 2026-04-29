#pragma once

#include <vector>

namespace lyra::runtime {

class RuntimeProcess;

class RuntimeEvent {
 public:
  RuntimeEvent() = default;

  RuntimeEvent(const RuntimeEvent&) = delete;
  auto operator=(const RuntimeEvent&) -> RuntimeEvent& = delete;
  RuntimeEvent(RuntimeEvent&&) = delete;
  auto operator=(RuntimeEvent&&) -> RuntimeEvent& = delete;
  ~RuntimeEvent() = default;

  void AddWaiter(RuntimeProcess& process);
  [[nodiscard]] auto TakeWaiters() -> std::vector<RuntimeProcess*>;

 private:
  std::vector<RuntimeProcess*> waiters_;
};

}  // namespace lyra::runtime
