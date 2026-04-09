#include "lyra/runtime/instance_event_state.hpp"

#include <format>
#include <utility>

#include "lyra/common/internal_error.hpp"

namespace lyra::runtime {

void RuntimeInstanceEventState::Init(uint32_t count) {
  local_event_count = count;
  slots.clear();
  slots.resize(count);
}

void RuntimeInstanceEventState::AddWaiter(
    uint32_t local_event_id, EventWaiter waiter) {
  if (local_event_id >= local_event_count) {
    throw common::InternalError(
        "RuntimeInstanceEventState::AddWaiter",
        std::format(
            "local_event_id {} out of range (count {})", local_event_id,
            local_event_count));
  }
  slots[local_event_id].waiters.push_back(std::move(waiter));
}

auto RuntimeInstanceEventState::ConsumeWaiters(uint32_t local_event_id)
    -> std::vector<EventWaiter> {
  if (local_event_id >= local_event_count) {
    throw common::InternalError(
        "RuntimeInstanceEventState::ConsumeWaiters",
        std::format(
            "local_event_id {} out of range (count {})", local_event_id,
            local_event_count));
  }
  auto out = std::move(slots[local_event_id].waiters);
  slots[local_event_id].waiters.clear();
  return out;
}

}  // namespace lyra::runtime
