#pragma once

#include <cstdint>

#include "lyra/common/local_slot_id.hpp"

namespace lyra::common {

// Resolved external-ref runtime binding record.
// One per external-ref recipe per owning instance.
// Carries object-first storage addressing and behavioral identity.
//
// target_byte_offset: body-relative byte offset within the target
// instance's inline storage region. Computed from the target body's
// layout during construction program extraction.
//
// target_instance_id + target_local_signal: object-local behavioral
// identity for dirty notification, trigger subscription, and NBA
// scheduling.
//
// This struct is the canonical runtime carrier for resolved ext-ref
// identity. It must not contain raw uint32_t for semantic fields,
// except target_instance_id which matches runtime::InstanceId layout
// but is declared as uint32_t to avoid a cross-layer dependency on
// runtime types from the common layer.
struct ResolvedExtRefBinding {
  // Body-relative byte offset into the target instance's inline storage.
  // Populated from body_layout.inline_offsets[target_local_signal] during
  // construction program extraction (after layout).
  uint32_t target_byte_offset = 0;
  // InstanceId.value of the target instance. Matches runtime::InstanceId
  // by layout (uint32_t). Stored here as uint32_t because common cannot
  // depend on runtime. Callers in the runtime layer cast via InstanceId{}.
  uint32_t target_instance_id = 0;
  LocalSlotId target_local_signal;
};

static_assert(sizeof(ResolvedExtRefBinding) == 12);
static_assert(offsetof(ResolvedExtRefBinding, target_byte_offset) == 0);
static_assert(offsetof(ResolvedExtRefBinding, target_instance_id) == 4);
static_assert(offsetof(ResolvedExtRefBinding, target_local_signal) == 8);

}  // namespace lyra::common
