#pragma once

#include <cstdint>

#include "lyra/common/local_slot_id.hpp"

namespace lyra::common {

// Serialized external-ref binding record for codegen transport.
// One per external-ref recipe per owning instance.
//
// Emitted as LLVM constants during construction program extraction.
// Consumed once by LyraConstructionResultSetExtRefBindings at runtime
// ingress, which resolves target_instance_id to a live RuntimeInstance*.
// Not stored in any runtime carrier after init.
//
// target_byte_offset: body-relative byte offset within the target
// instance's inline storage region. Computed from the target body's
// layout during construction program extraction (after layout).
//
// target_instance_id: construction-order object index of the target
// instance. Matches runtime::InstanceId by layout (uint32_t). Stored
// as uint32_t because common cannot depend on runtime types.
struct SerializedExtRefBinding {
  uint32_t target_byte_offset = 0;
  uint32_t target_instance_id = 0;
  LocalSlotId target_local_signal;
};

static_assert(sizeof(SerializedExtRefBinding) == 12);
static_assert(offsetof(SerializedExtRefBinding, target_byte_offset) == 0);
static_assert(offsetof(SerializedExtRefBinding, target_instance_id) == 4);
static_assert(offsetof(SerializedExtRefBinding, target_local_signal) == 8);

}  // namespace lyra::common
