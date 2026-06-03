#include "lyra/runtime/event.hpp"

#include <utility>
#include <vector>

namespace lyra::runtime {

void RuntimeEvent::AddWaiter(CoroutineHandle waiter) {
  waiters_.push_back(waiter);
}

auto RuntimeEvent::TakeWaiters() -> std::vector<CoroutineHandle> {
  return std::exchange(waiters_, {});
}

}  // namespace lyra::runtime
