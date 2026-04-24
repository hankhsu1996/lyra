#pragma once

#include <cstddef>
#include <cstdint>
#include <type_traits>

namespace lyra::runtime {

// Shared process-state schema.
//
// One entry per unique schema identity (body_id, proc_within_body).
// All simulation processes with the same schema identity share identical
// state size, alignment, and initialization behavior.
//
// Codegen emits as a global constant array. Both the old constructor-record
// allocator and the new runtime Constructor consume these to allocate and
// initialize process states.
struct ProcessStateSchema {
  // Complete state object size (header + frame), not frame alone.
  uint64_t state_size;
  // Complete state object alignment requirement.
  uint64_t state_align;
  // Per-schema 4-state frame initialization function.
  // Takes a pointer to the complete state object and internally derives
  // the frame pointer. nullptr if no 4-state initialization is needed.
  void (*frame_init)(void*);
};

static_assert(sizeof(ProcessStateSchema) == 24);
static_assert(offsetof(ProcessStateSchema, state_size) == 0);
static_assert(offsetof(ProcessStateSchema, state_align) == 8);
static_assert(offsetof(ProcessStateSchema, frame_init) == 16);
static_assert(std::is_trivially_copyable_v<ProcessStateSchema>);
static_assert(std::is_standard_layout_v<ProcessStateSchema>);

}  // namespace lyra::runtime
