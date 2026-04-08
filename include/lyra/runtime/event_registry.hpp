#pragma once

#include <cstdint>
#include <unordered_map>
#include <vector>

namespace lyra::runtime {

// Typed runtime identity for a named event object.
//
// A named event is an instance-owned synchronization primitive: two instances
// of the same module body have independent event objects even though they
// share the same body-local EventId at compile time. At runtime, the pair
// (instance_id, local_event_id) uniquely identifies a single event object,
// where local_event_id is the body-local ordinal for the instance's active
// compiled body.
struct EventObjectKey {
  uint32_t instance_id;
  uint32_t local_event_id;

  auto operator==(const EventObjectKey&) const -> bool = default;
};

struct EventObjectKeyHash {
  auto operator()(const EventObjectKey& key) const -> size_t {
    return std::hash<uint64_t>{}(
        (static_cast<uint64_t>(key.instance_id) << 32) | key.local_event_id);
  }
};

struct EventWaiter {
  uint32_t process_id;
  uint32_t instance_id;
  uint32_t resume_block;
};

// Runtime state for one event object (one per instance x body-local event).
struct EventRuntimeState {
  std::vector<EventWaiter> waiters;
};

// Named event synchronization registry.
//
// Events are instance-owned objects: two instances of the same module body
// have independent event objects even though they share the same body-local
// EventId. The registry keys by EventObjectKey to maintain this identity
// separation.
class EventRegistry {
 public:
  // Register a waiter on a specific event object.
  void AddWaiter(EventObjectKey key, EventWaiter waiter) {
    events_[key].waiters.push_back(waiter);
  }

  // Consume and return all waiters for a specific event object (one-shot).
  // Erases the entry to keep the map clean after consumption.
  auto ConsumeWaiters(EventObjectKey key) -> std::vector<EventWaiter> {
    auto it = events_.find(key);
    if (it == events_.end()) {
      return {};
    }
    auto waiters = std::move(it->second.waiters);
    events_.erase(it);
    return waiters;
  }

 private:
  std::unordered_map<EventObjectKey, EventRuntimeState, EventObjectKeyHash>
      events_;
};

}  // namespace lyra::runtime
