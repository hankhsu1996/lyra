#pragma once

#include <cstdint>
#include <span>

#include "lyra/common/index_plan.hpp"
#include "lyra/runtime/engine_types.hpp"
#include "lyra/runtime/trigger_record.hpp"
#include "lyra/runtime/wait_site.hpp"

namespace lyra::runtime {

// Semantic process scheduling requests. Produced by the process envelope
// after decoding the raw suspend protocol. Consumed by engine scheduling
// primitives. These are runtime-semantic types, not transport artifacts.

struct DelayRequest {
  ResumePoint resume = {};
  uint64_t ticks = 0;
};

// Late-bound rebind payload: headers index into plan_ops and dep_slots
// via start/count pairs. These three arrays are always consumed together
// and only by the subscription install path's rebind loop.
// Empty headers means no late-bound data.
struct LateBoundData {
  std::span<const LateBoundHeader> headers = {};
  std::span<const IndexPlanOp> plan_ops = {};
  std::span<const DepSignalRecord> dep_slots = {};
};

struct WaitRequest {
  ResumePoint resume = {};
  WaitSiteId wait_site_id = kInvalidWaitSiteId;
  std::span<const WaitTriggerRecord> triggers = {};
  LateBoundData late_bound = {};
};

struct RepeatRequest {
  ResumePoint resume = {};
};

struct EventWaitRequest {
  ResumePoint resume = {};
  uint32_t event_id = 0;
};

}  // namespace lyra::runtime
