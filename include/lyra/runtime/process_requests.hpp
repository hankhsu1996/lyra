#pragma once

#include <cstdint>
#include <span>

#include "lyra/runtime/engine_types.hpp"
#include "lyra/runtime/trigger_record.hpp"
#include "lyra/runtime/wait_site.hpp"

namespace lyra::runtime {

struct IndexPlanOp;

// Semantic process scheduling requests. Produced by the process envelope
// after decoding the raw suspend protocol. Consumed by engine scheduling
// primitives. These are runtime-semantic types, not transport artifacts.

struct DelayRequest {
  ResumePoint resume = {};
  uint64_t ticks = 0;
};

struct WaitRequest {
  ResumePoint resume = {};
  WaitSiteId wait_site_id = kInvalidWaitSiteId;
  std::span<const WaitTriggerRecord> triggers = {};
  std::span<const LateBoundHeader> late_bound = {};
  std::span<const IndexPlanOp> plan_ops = {};
  std::span<const DepSignalRecord> dep_slots = {};
};

struct RepeatRequest {
  ResumePoint resume = {};
};

struct EventWaitRequest {
  ResumePoint resume = {};
  uint32_t event_id = 0;
};

}  // namespace lyra::runtime
