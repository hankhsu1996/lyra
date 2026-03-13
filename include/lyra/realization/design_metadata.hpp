#pragma once

#include <cstdint>
#include <string>
#include <vector>

namespace lyra::realization {

struct MetaWordTable {
  std::vector<uint32_t> words;
  std::vector<char> pool;
};

// Input facts extracted from LLVM layout for each design slot.
struct SlotMetaInput {
  uint32_t byte_offset = 0;
  uint32_t total_bytes = 0;
  uint32_t storage_kind = 0;
  uint32_t value_offset = 0;
  uint32_t value_bytes = 0;
  uint32_t unk_offset = 0;
  uint32_t unk_bytes = 0;
};

// Runtime-shaped connection descriptor. Already final form, passed through.
struct ConnectionDescriptorEntry {
  uint32_t src_byte_offset = 0;
  uint32_t dst_byte_offset = 0;
  uint32_t byte_size = 0;
  uint32_t dst_slot_id = 0;
  uint32_t trigger_slot_id = 0;
  uint8_t trigger_edge = 0;
  uint8_t trigger_bit_index = 0;
  uint32_t trigger_byte_offset = 0;
  uint32_t trigger_byte_size = 0;
};

// Input facts for a scheduled module process.
// scheduled_process_index is the canonical 0-based index into the process meta
// table, assigned at extraction time. All cross-table references use this
// index.
struct ScheduledProcessInput {
  uint32_t scheduled_process_index = 0;
  uint32_t module_index = 0;
  uint32_t kind_packed = 0;
  std::string instance_path;
  std::string file;
  uint32_t line = 0;
  uint32_t col = 0;
};

// Runtime-facing trigger observation for a comb kernel input slot.
// byte_size == 0 means full-slot (no sub-slot narrowing).
struct CombTriggerInput {
  uint32_t slot_id = 0;
  uint32_t byte_offset = 0;
  uint32_t byte_size = 0;
};

// Input facts for a combinational kernel.
// scheduled_process_index is the canonical index into the process meta table
// (0-based, counting from the first module process after init processes).
struct CombKernelInput {
  uint32_t scheduled_process_index = 0;
  std::vector<CombTriggerInput> triggers;
  bool has_self_edge = false;
};

// Input facts for a loop site.
// loop_site_index is the canonical 0-based row index, assigned at extraction
// time. Serialized in this order.
struct LoopSiteInput {
  uint32_t loop_site_index = 0;
  std::string file;
  uint32_t line = 0;
  uint32_t col = 0;
};

// Semantic inputs for design metadata construction.
// All vectors preserve their input order; link serializes in that order.
struct DesignMetadataInputs {
  std::vector<SlotMetaInput> slot_meta;
  std::vector<ScheduledProcessInput> scheduled_processes;
  std::vector<LoopSiteInput> loop_sites;
  std::vector<ConnectionDescriptorEntry> connection_descriptors;
  std::vector<CombKernelInput> comb_kernels;
  std::vector<std::string> instance_paths;
};

// Fully serialized runtime metadata artifact.
// All tables are runtime-shaped; no further packing needed by the emitter.
struct DesignMetadata {
  std::vector<uint32_t> slot_meta_words;
  MetaWordTable process_meta;
  MetaWordTable loop_site_meta;
  std::vector<ConnectionDescriptorEntry> connection_descriptors;
  std::vector<uint32_t> comb_kernel_words;
  std::vector<std::string> instance_paths;
};

}  // namespace lyra::realization
