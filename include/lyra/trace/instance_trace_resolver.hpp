#pragma once

#include <cstdint>

namespace lyra::runtime {
struct RuntimeInstance;
}

namespace lyra::trace {

// Explicit lookup service for resolving instance_id to RuntimeInstance
// in trace sinks. Decouples sinks from Engine's internal instance storage
// layout. Sinks must not assume instance_id is a dense vector index.
//
// Lifetime: the resolver must outlive all sinks that reference it.
// Typically owned by Engine and passed to sinks at initialization.
class InstanceTraceResolver {
 public:
  InstanceTraceResolver() = default;
  virtual ~InstanceTraceResolver() = default;
  InstanceTraceResolver(const InstanceTraceResolver&) = default;
  auto operator=(const InstanceTraceResolver&)
      -> InstanceTraceResolver& = default;
  InstanceTraceResolver(InstanceTraceResolver&&) = default;
  auto operator=(InstanceTraceResolver&&) -> InstanceTraceResolver& = default;

  // Returns the RuntimeInstance for the given instance_id, or nullptr
  // if the id is not valid. Implementations must be safe to call from
  // the trace dispatch path (no allocation, no locking).
  [[nodiscard]] virtual auto FindInstance(uint32_t instance_id) const
      -> const runtime::RuntimeInstance* = 0;
};

}  // namespace lyra::trace
