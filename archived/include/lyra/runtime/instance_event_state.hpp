#pragma once

#include <cstdint>
#include <vector>

namespace lyra::runtime {

struct RuntimeInstance;
struct RuntimeProcess;

// Waiter record for a named event. Stored per-event-slot on the owning
// instance. Consumed (moved out) when the event is triggered.
struct EventWaiter {
  RuntimeProcess* process = nullptr;
  RuntimeInstance* instance = nullptr;
  uint32_t resume_block = 0;
};

// Per-event runtime state within an instance. One slot per body-local
// named event declaration. Extensible for L8b .triggered state.
struct InstanceEventSlotState {
  std::vector<EventWaiter> waiters;
};

// Instance-owned named event runtime state.
//
// Each RuntimeInstance owns one of these. Sized from the body-local event
// count during InitModuleInstancesFromBundles, following the same
// initialization pattern as RuntimeInstanceObservability.
//
// Replaces the prior global EventRegistry model where event state was
// keyed by instance + local_event_id in an engine-owned hash map.
struct RuntimeInstanceEventState {
  uint32_t local_event_count = 0;
  std::vector<InstanceEventSlotState> slots;

  // Size the event state from the body-local event count.
  void Init(uint32_t count);

  // Register a waiter on a specific event slot.
  // Validates local_event_id is in range.
  void AddWaiter(uint32_t local_event_id, EventWaiter waiter);

  // Consume and return all waiters for a specific event slot (one-shot).
  // Moves the waiter list out and leaves the slot empty.
  // Validates local_event_id is in range.
  auto ConsumeWaiters(uint32_t local_event_id) -> std::vector<EventWaiter>;
};

}  // namespace lyra::runtime
