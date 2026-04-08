#pragma once

#include <cstdint>
#include <string>
#include <vector>

#include "lyra/runtime/engine_subscriptions.hpp"
#include "lyra/runtime/engine_types.hpp"

namespace lyra::runtime {

// Canonical per-process wait-state classification.
// Derived from SuspendTag in the process's SuspendRecord (the runtime's
// canonical suspend state), enriched by subscription and queue membership.
enum class ProcessWaitKind : uint8_t {
  kRunning,
  kReady,
  kSuspendedDelay,
  kSuspendedEdge,
  kSuspendedChange,
  kSuspendedMulti,
  kSuspendedRepeat,
  kSuspendedEvent,
  kSuspendedUnknown,
  kFinished,
};

// Why the simulation stopped. Stored as canonical state in Engine, set
// exactly once at the point where termination reason becomes known.
enum class SimulationEndReason : uint8_t {
  kFinish,
  kDeltaCycleLimit,
  kMaxTimeReached,
  kEmptyQueues,
  kTrap,
};

// Summary of one installed subscription for diagnostic rendering.
// Self-contained: includes signal name so the renderer does not need
// access to live engine registries.
struct SubscriptionSummary {
  uint32_t slot_id = 0;
  std::string signal_name;
  SubKind kind = SubKind::kEdge;
  EdgeBucket edge_bucket = EdgeBucket::kPosedge;
  bool is_active = false;
  // Edge-specific state (meaningful only for kEdge).
  uint8_t group_last_bit = 0;
  uint8_t current_bit = 0;
};

// Point-in-time snapshot of the entire scheduler state.
// Self-contained: all identity and diagnostic data is resolved at snapshot
// time. The renderer is a pure function of this struct.
struct SchedulerSnapshot {
  SimTime time = 0;
  uint32_t delta = 0;
  SimulationEndReason end_reason = SimulationEndReason::kFinish;

  uint32_t active_queue_size = 0;
  uint32_t inactive_queue_size = 0;
  uint32_t next_delta_queue_size = 0;
  uint32_t delay_queue_entry_count = 0;

  struct ProcessEntry {
    uint32_t process_id = 0;
    std::string identity;
    ProcessWaitKind wait_kind = ProcessWaitKind::kFinished;
    // For kSuspendedDelay.
    SimTime delay_target_time = 0;
    // For kSuspendedEdge/Change/Multi.
    std::vector<SubscriptionSummary> subscriptions;
  };
  std::vector<ProcessEntry> suspended_processes;
};

}  // namespace lyra::runtime
