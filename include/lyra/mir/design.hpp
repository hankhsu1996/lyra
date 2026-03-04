#pragma once

#include <cstddef>
#include <cstdint>
#include <variant>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "lyra/common/integral_constant.hpp"
#include "lyra/common/symbol_types.hpp"
#include "lyra/common/type.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/instance.hpp"
#include "lyra/mir/module.hpp"
#include "lyra/mir/package.hpp"

namespace lyra::mir {

enum class SlotKind : uint8_t { kVariable, kNet, kParamConst };

struct SlotDesc {
  TypeId type;
  SlotKind kind = SlotKind::kVariable;
};

// Initialization entry for a promoted parameter slot.
// Slot is identified by absolute design slot index.
// Type is read from design.slots[slot_id].type (single source of truth).
struct ParamInitEntry {
  uint32_t slot_id;
  IntegralConstant value;
};

using DesignElement = std::variant<Module, Package>;

// Port connection record with asymmetric representation for different kinds.
// Source of truth for all port connections - alias_map is derived from this.
struct PortConnection {
  enum class Kind : int32_t {
    kDriveParentToChild,  // input: parent expr -> child port (via process)
    kDriveChildToParent,  // output variable: child port -> parent place
    kAlias,               // output/inout net: true identity
  };

  Kind kind = Kind::kDriveParentToChild;
  SymbolId child_port_sym;       // Child's port backing variable
  SymbolId parent_instance_sym;  // Parent module's instance symbol

  // For kDriveChildToParent and kAlias: both places are meaningful.
  // For kDriveParentToChild: only child_place is meaningful here;
  //   the source is the connection process itself (not a simple place).
  PlaceId child_place;   // Always valid
  PlaceId parent_place;  // Valid for kDriveChildToParent, kAlias
};

struct Design {
  std::vector<DesignElement> elements;
  size_t num_design_slots = 0;
  // Slot descriptors: indexed by design slot ID, contains type and kind for
  // each slot. Ordering is ABI: packages first (in element order), then all
  // module instances (in BFS elaboration order).
  // slots.size() == num_design_slots.
  std::vector<SlotDesc> slots;
  std::vector<ProcessId> init_processes;
  // Functions dynamically generated during lowering (e.g., strobe thunks from
  // init processes). These are not associated with any specific module.
  std::vector<FunctionId> generated_functions;

  // Global precision power for %t formatting (e.g., -12 for 1ps).
  // Set from compilation context, used by $timeformat defaults.
  int8_t global_precision_power = -9;

  // Source of truth for all port connections.
  std::vector<PortConnection> port_connections;

  // Wiring processes (separate from module processes).
  // These implement kDriveParentToChild and kDriveChildToParent semantics.
  std::vector<ProcessId> connection_processes;

  // DERIVED: acceleration structure for kAlias only.
  // INVARIANT: entry exists iff PortConnection{kAlias} exists for that slot.
  // Written ONLY in ApplyBindings, nowhere else.
  // Key: child port's SlotId (must be kDesign root with no projections)
  // Value: parent's PlaceId (may have projections, must resolve to kDesign)
  absl::flat_hash_map<SlotId, PlaceId> alias_map;

  // Instance table for %m support.
  // Index = instance_id, same order as all_instances during elaboration.
  InstanceTable instance_table;

  // Per-module-instance slot ranges (parallel to module elements only).
  // Populated during HIR->MIR lowering from DesignDeclarations.
  struct InstanceSlotRange {
    uint32_t slot_begin = 0;
    uint32_t slot_count = 0;
  };
  std::vector<InstanceSlotRange> instance_slot_ranges;
  // Per-module-instance def keys (parallel to instance_slot_ranges).
  std::vector<uint64_t> module_def_keys;

  // Per-module-instance param init entries (parallel to instance_slot_ranges).
  // instance_param_inits[module_idx] = entries for that instance.
  std::vector<std::vector<ParamInitEntry>> instance_param_inits;
};

}  // namespace lyra::mir
