#pragma once

#include <cstdint>
#include <string>
#include <string_view>
#include <vector>

#include "lyra/common/symbol_types.hpp"

namespace lyra::mir {

// Entry in the instance table for %m support.
// Each entry represents a module instance in the design hierarchy.
struct InstanceEntry {
  std::string full_path;  // Hierarchical path (e.g., "top.u_cpu.u_alu")
  SymbolId instance_sym;  // Cross-reference for debugging
};

// Table of all module instances in the design, indexed by instance_id.
// The instance_id is used in ProcessHandle to enable %m formatting.
struct InstanceTable {
  std::vector<InstanceEntry> entries;

  // Look up the hierarchical path for an instance.
  // Returns empty string_view for invalid instance_id.
  [[nodiscard]] auto GetPath(uint32_t instance_id) const -> std::string_view {
    if (instance_id >= entries.size()) {
      return "";
    }
    return entries[instance_id].full_path;
  }
};

}  // namespace lyra::mir
