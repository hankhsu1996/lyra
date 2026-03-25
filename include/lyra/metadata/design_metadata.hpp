#pragma once

#include <cstdint>
#include <string>
#include <vector>

namespace lyra::metadata {

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
  uint32_t storage_owner_slot_id = 0;
};

// Semantic origin of a kernelized connection.
// kPortBinding: created from a port-binding connection process.
// kContinuousAssign: created from a module-internal continuous assign.
// Today all kernelized connections are kPortBinding because continuous
// assigns are lowered as always_comb and kernelized as CombKernels.
// The second value exists to make the provenance model explicit and
// ready for future use if continuous assigns ever become connections.
enum class ConnectionKernelOrigin : uint8_t {
  kPortBinding,
  kContinuousAssign,
};

// Runtime-shaped connection descriptor with compile-time provenance.
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
  // Compile-time-only provenance. Not serialized to runtime ABI.
  ConnectionKernelOrigin origin = ConnectionKernelOrigin::kPortBinding;
};

// Input facts for a back-edge site.
// back_edge_site_index is the canonical 0-based row index, assigned at
// extraction time. Serialized in this order.
struct BackEdgeSiteInput {
  uint32_t back_edge_site_index = 0;
  std::string file;
  uint32_t line = 0;
  uint32_t col = 0;
};

// Final trace-signal metadata input for one design slot.
// Assembled at metadata lowering time from compile-owned provenance.
struct TraceSignalMetaInput {
  std::string hierarchical_name;
  uint32_t bit_width = 0;
  uint32_t trace_kind = 0;
  uint32_t storage_owner_slot_id = 0;
};

// Semantic inputs for design metadata construction.
// All vectors preserve their input order; link serializes in that order.
struct DesignMetadataInputs {
  std::vector<SlotMetaInput> slot_meta;
  std::vector<BackEdgeSiteInput> back_edge_sites;
  std::vector<ConnectionDescriptorEntry> connection_descriptors;
  std::vector<std::string> instance_paths;
  std::vector<TraceSignalMetaInput> trace_signal_meta;
};

// Fully serialized runtime metadata artifact.
// All tables are runtime-shaped; no further packing needed by the emitter.
struct DesignMetadata {
  std::vector<uint32_t> slot_meta_words;
  MetaWordTable back_edge_site_meta;
  std::vector<ConnectionDescriptorEntry> connection_descriptors;
  std::vector<std::string> instance_paths;
  MetaWordTable trace_signal_meta;
};

}  // namespace lyra::metadata
