#pragma once

#include <cstdint>
#include <span>

#include "lyra/runtime/process_schema.hpp"

namespace lyra::runtime {

// Allocate a packed buffer of process frames from a sequence of schema indices.
//
// Each entry in schema_indices is an index into the schemas array. For each
// entry, a state region of (state_size, state_align) is allocated from a
// single packed buffer, zeroed, and initialized via frame_init if non-null.
//
// Returns the packed buffer pointer (caller frees via
// LyraDestroyProcessStates). states_out[i] is set to the allocated state
// pointer for schema_indices[i].
//
// This is the neutral allocation primitive used by both the old
// constructor-record path and the new Constructor object.
auto AllocateProcessFrames(
    std::span<void*> states_out, std::span<const uint32_t> schema_indices,
    std::span<const ProcessStateSchema> schemas) -> void*;

// Free a packed buffer returned by AllocateProcessFrames.
// Accepts nullptr (no-op).
void FreePackedBuffer(void* packed_buffer);

}  // namespace lyra::runtime
