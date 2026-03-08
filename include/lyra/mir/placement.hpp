#pragma once

#include <cstdint>
#include <vector>

#include "lyra/common/module_identity.hpp"
#include "lyra/common/symbol_types.hpp"

namespace lyra::mir {

// Transitional placement artifact for one module instance in DesignState.
//
// Source of truth for where an instance's specialization-local storage lives
// in the design-global DesignState arena. Placement is computed by a running
// base counter over specialization bodies, not copied from legacy slot tables.
//
// Relationship to other artifacts:
// - ModuleBody.slots defines the specialization-local storage interface
// - InstancePlacement maps that interface to a design-global base offset
// - Design::instance_slot_ranges is derived compatibility data (until C3)
struct InstancePlacement {
  SymbolId instance_sym;
  common::ModuleSpecId spec_id;
  uint32_t design_state_base_slot = 0;
  uint32_t slot_count = 0;
};

// Owning aggregate for all instance placements in a design.
// Indexed by module-instance index (module-only elements in BFS elaboration
// order). NOT parallel to Design::elements (which also contains packages).
struct PlacementMap {
  std::vector<InstancePlacement> instances;
};

// Look up placement for a module instance by its index.
auto GetInstancePlacement(const PlacementMap& map, uint32_t module_index)
    -> const InstancePlacement&;

// Convenience: get the design-state base slot for a module instance.
auto GetInstanceBaseSlot(const PlacementMap& map, uint32_t module_index)
    -> uint32_t;

}  // namespace lyra::mir
