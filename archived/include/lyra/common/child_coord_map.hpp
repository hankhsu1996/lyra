#pragma once

#include <string>
#include <unordered_map>
#include <vector>

#include "lyra/common/module_identity.hpp"
#include "lyra/common/selection_step.hpp"

namespace lyra::common {

// Lightweight child-instance-to-repertoire-coord map.
// Built during AST-to-HIR from the definition repertoire descriptor.
// Consumed by design_lower.cpp for durable child-site identity.
//
// Key: (ModuleDefId, child_instance_name) -> RepertoireCoord.
// The child instance name is the slang instance name (e.g., "u1", "child[0]").
struct ChildCoordEntry {
  std::string inst_name;
  RepertoireCoord coord;
};

struct ModuleDefIdHash {
  auto operator()(const ModuleDefId& id) const noexcept -> size_t {
    return std::hash<uint32_t>{}(id.value);
  }
};

// Per-definition map of child instance names to repertoire coordinates.
// Indexed by ModuleDefId of the parent definition.
using ChildCoordMap = std::unordered_map<
    ModuleDefId, std::vector<ChildCoordEntry>, ModuleDefIdHash>;

}  // namespace lyra::common
