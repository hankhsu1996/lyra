#pragma once

#include <cstdint>
#include <vector>

namespace lyra::common {

using ModuleInstanceIndex = uint32_t;

struct ModuleDefId {
  uint32_t value;

  auto operator==(const ModuleDefId&) const -> bool = default;
  auto operator<=>(const ModuleDefId&) const = default;
};

struct StructuralFingerprint {
  uint64_t value;

  auto operator==(const StructuralFingerprint&) const -> bool = default;
  auto operator<=>(const StructuralFingerprint&) const = default;
};

struct ModuleSpecId {
  ModuleDefId def_id;
  StructuralFingerprint fingerprint;

  auto operator==(const ModuleSpecId&) const -> bool = default;
  auto operator<=>(const ModuleSpecId&) const = default;
};

struct SpecializationGroup {
  ModuleSpecId spec_id;
  std::vector<ModuleInstanceIndex> instance_indices;
};

// Complete grouping of all module instances by specialization.
//
// Indexing contract: `spec_id_by_instance[i]` corresponds to
// `all_instances[i]` in design.cpp, which is the BFS-discovered,
// hierarchical-path-sorted instance order.  This same order defines
// `design.elements` (module entries only) and `design.instance_slot_ranges`.
struct SpecializationMap {
  std::vector<ModuleSpecId> spec_id_by_instance;

  // Groups sorted deterministically by (ModuleDefId, StructuralFingerprint).
  std::vector<SpecializationGroup> groups;
};

}  // namespace lyra::common
