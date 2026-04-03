#pragma once

#include <cstdint>

#include "lyra/runtime/signal_coord.hpp"

namespace lyra::runtime {
struct RuntimeInstance;
}

namespace lyra::trace {

// Explicit lookup service for resolving InstanceId to RuntimeInstance
// in trace sinks. Decouples sinks from Engine's internal instance storage
// layout. Sinks must not assume InstanceId maps to a dense vector index.
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
  [[nodiscard]] virtual auto FindInstance(runtime::InstanceId instance_id) const
      -> const runtime::RuntimeInstance* = 0;
};

}  // namespace lyra::trace
