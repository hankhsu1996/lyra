#pragma once

#include <cstddef>
#include <cstdint>
#include <type_traits>

namespace lyra::runtime {

// Shared process-state construction schema.
//
// One entry per unique schema identity (body_id, proc_within_body).
// All simulation processes with the same schema identity share identical
// state size, alignment, and initialization behavior.
//
// Codegen emits as a global constant array. Runtime constructor consumes
// these to allocate and initialize process states.
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

// Per-simulation-process constructor routing record.
//
// One record per simulation process (connection + module, post-init).
// This is the canonical per-process constructor contract. Any
// constructor-time instance-local fact must live here, never in a
// side array.
struct ProcessConstructorRecord {
  // Index into the ProcessStateSchema array.
  uint32_t schema_index;
};

static_assert(sizeof(ProcessConstructorRecord) == 4);
static_assert(offsetof(ProcessConstructorRecord, schema_index) == 0);
static_assert(std::is_trivially_copyable_v<ProcessConstructorRecord>);
static_assert(std::is_standard_layout_v<ProcessConstructorRecord>);

// Allocate and initialize process states for all simulation processes.
//
// - states_out: pre-allocated array of num_processes pointers (caller owns)
// - num_processes: total simulation processes (connection + module, post-init)
// - state_schemas: per-schema metadata array
// - num_state_schemas: schema count (used for index validation)
// - records: per-process constructor routing records
//
// Returns heap-allocated packed buffer.
// Caller frees via LyraDestroyProcessStates.
//
// Edge cases:
// - If num_processes == 0, returns nullptr. states_out is untouched.
//   LyraDestroyProcessStates accepts nullptr (no-op).
// - All entries in states_out are set to valid non-null pointers,
//   even for zero-sized schemas. Buffer allocation rounds up to 1 byte
//   minimum to ensure pointer stability.
// - Multiple zero-sized schemas may receive identical pointers into the
//   buffer. This is acceptable: non-null pointer stability is provided,
//   identical pointers are allowed, and no code may rely on uniqueness
//   or writable storage for zero-sized states.
extern "C" void* LyraConstructProcessStates(
    void** states_out, uint32_t num_processes,
    const ProcessStateSchema* state_schemas, uint32_t num_state_schemas,
    const ProcessConstructorRecord* records);

// Free the packed buffer returned by LyraConstructProcessStates.
// Accepts nullptr (no-op).
extern "C" void LyraDestroyProcessStates(void* packed_buffer);

}  // namespace lyra::runtime
