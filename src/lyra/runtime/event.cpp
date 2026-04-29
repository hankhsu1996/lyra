#include "lyra/runtime/event.hpp"

#include <utility>
#include <vector>

namespace lyra::runtime {

void RuntimeEvent::AddWaiter(RuntimeProcess& process) {
  waiters_.push_back(&process);
}

auto RuntimeEvent::TakeWaiters() -> std::vector<RuntimeProcess*> {
  return std::exchange(waiters_, {});
}

}  // namespace lyra::runtime
