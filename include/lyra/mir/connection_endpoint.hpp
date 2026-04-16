#pragma once

#include <cstdint>

#include "lyra/common/edge_kind.hpp"
#include "lyra/common/local_slot_id.hpp"
#include "lyra/common/object_index.hpp"
#include "lyra/common/type.hpp"
#include "lyra/mir/port_connection.hpp"

namespace lyra::mir {

// Bound location: a specific slot on a specific object in the
// construction topology. Shared identity type for external ref
// bindings, connection endpoints, and resolution artifacts.
struct BoundEndpoint {
  common::ObjectIndex object_index;
  common::LocalSlotId local_slot;
};

// Trigger observation metadata for connection kernels.
struct ResolvedObservation {
  uint32_t byte_offset = 0;
  uint32_t byte_size = 0;
  uint8_t bit_index = 0;
};

// A fully-bindable ConnectionRecipe resolved against the construction
// topology. All endpoints are concrete (ObjectIndex + LocalSlotId).
// Produced by BindConnectionRecipe for the subset of recipes where
// source and trigger are slot-based. The recipe path does not produce
// trigger observations: all bound connections have null observation,
// enforced by BindConnectionRecipe's precondition.
struct BoundConnection {
  uint32_t recipe_index = 0;
  PortConnection::Kind kind = PortConnection::Kind::kDriveParentToChild;
  common::ObjectIndex parent_object_index;
  BoundEndpoint child_target;
  BoundEndpoint parent_source;
  BoundEndpoint trigger;
  common::EdgeKind trigger_edge = common::EdgeKind::kAnyChange;
  TypeId result_type;
};

}  // namespace lyra::mir
