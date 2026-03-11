#pragma once

#include <cstdint>
#include <vector>

#include "lyra/common/integral_constant.hpp"
#include "lyra/common/module_identity.hpp"
#include "lyra/common/symbol_types.hpp"

namespace lyra::mir {

// One constant-valued slot initialization within an InstanceConstBlock.
// `body_local_slot` is in body-local slot space (0-based within the
// specialization body). Consumers resolve to design-global slot IDs by
// combining with the matching InstancePlacement::design_state_base_slot.
struct ConstSlotInit {
  uint32_t body_local_slot;
  IntegralConstant value;
};

// Per-instance constant initialization data for value-only parameters.
// Paired 1:1 with InstancePlacement entries (same index = same instance).
// This is a realization artifact: specialization owns the code shape,
// realization owns the per-instance constant data.
struct InstanceConstBlock {
  std::vector<ConstSlotInit> slot_inits;
};

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
//
// Invariant: instances.size() == const_blocks.size().
// Each const_blocks[i] belongs to the instance at instances[i].
struct PlacementMap {
  std::vector<InstancePlacement> instances;
  // Per-instance constant initialization data for value-only parameters.
  // Parallel to `instances`: const_blocks[i] is paired with instances[i].
  std::vector<InstanceConstBlock> const_blocks;
};

// Look up placement for a module instance by its index.
auto GetInstancePlacement(const PlacementMap& map, uint32_t module_index)
    -> const InstancePlacement&;

// Convenience: get the design-state base slot for a module instance.
auto GetInstanceBaseSlot(const PlacementMap& map, uint32_t module_index)
    -> uint32_t;

}  // namespace lyra::mir
