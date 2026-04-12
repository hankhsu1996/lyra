#pragma once

#include <cstdint>
#include <string>
#include <vector>

namespace lyra::metadata {

struct MetaWordTable {
  std::vector<uint32_t> words;
  std::vector<char> pool;
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
  uint32_t src_slot_id = 0;
  uint32_t dst_slot_id = 0;
  uint32_t byte_size = 0;
  uint32_t trigger_slot_id = 0;
  uint8_t trigger_edge = 0;
  uint8_t trigger_bit_index = 0;
  uint32_t trigger_byte_offset = 0;
  uint32_t trigger_byte_size = 0;
  // R5: Typed destination identity. When dst_is_local is true,
  // dst_instance_id and dst_local_id carry the owning instance and
  // body-local signal identity. Populated at layout time from design
  // slot ownership. Runtime decodes into BatchedConnectionDst variant.
  uint8_t dst_is_local = 0;
  uint32_t dst_instance_id = 0;
  uint32_t dst_local_id = 0;
  // R5: Typed trigger identity. Same pattern as destination.
  uint8_t trigger_is_local = 0;
  uint32_t trigger_instance_id = 0;
  uint32_t trigger_local_id = 0;
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

// Semantic inputs for design metadata construction.
// All vectors preserve their input order; link serializes in that order.
struct DesignMetadataInputs {
  std::vector<BackEdgeSiteInput> back_edge_sites;
  std::vector<ConnectionDescriptorEntry> connection_descriptors;
};

// Fully serialized runtime metadata artifact.
// All tables are runtime-shaped; no further packing needed by the emitter.
struct DesignMetadata {
  MetaWordTable back_edge_site_meta;
  std::vector<ConnectionDescriptorEntry> connection_descriptors;
};

}  // namespace lyra::metadata
