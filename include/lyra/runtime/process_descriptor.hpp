#pragma once

#include <cstddef>
#include <cstdint>

namespace lyra::runtime {

// Canonical process descriptor entry layout.
//
// This header is the authoritative definition. Codegen
// (GetDescriptorEntryType in emit_design_main.cpp) and runtime
// (simulation.cpp, engine_scheduler_fixpoint.cpp) must both conform.
// Hard assertions below enforce the binary contract.
//
// Descriptors are consumed at init time to populate process frame
// headers. After init, dispatch paths read from frame headers, not
// from descriptors.
struct ProcessDescriptorEntry {
  void* shared_body;
  uint64_t base_byte_offset;
  uint32_t instance_id;
  uint32_t signal_id_offset;
};

static_assert(
    sizeof(ProcessDescriptorEntry) == 24,
    "descriptor entry size must match LLVM struct layout");
static_assert(offsetof(ProcessDescriptorEntry, shared_body) == 0);
static_assert(offsetof(ProcessDescriptorEntry, base_byte_offset) == 8);
static_assert(offsetof(ProcessDescriptorEntry, instance_id) == 16);
static_assert(offsetof(ProcessDescriptorEntry, signal_id_offset) == 20);

}  // namespace lyra::runtime
