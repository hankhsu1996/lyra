#pragma once

#include <cstddef>
#include <cstdint>
#include <type_traits>

#include "lyra/runtime/process_schema.hpp"

namespace lyra::runtime {

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
