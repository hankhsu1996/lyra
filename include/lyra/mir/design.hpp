#pragma once

#include <cstddef>
#include <cstdint>
#include <variant>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "lyra/common/symbol_types.hpp"
#include "lyra/common/type.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/module.hpp"
#include "lyra/mir/package.hpp"

namespace lyra::mir {

using DesignElement = std::variant<Module, Package>;

// Port connection record with asymmetric representation for different kinds.
// Source of truth for all port connections - alias_map is derived from this.
struct PortConnection {
  enum class Kind : int32_t {
    kDriveParentToChild,  // input: parent expr → child port (via process)
    kDriveChildToParent,  // output variable: child port → parent place
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
  // Slot table: indexed by design slot ID, contains TypeId for each slot.
  // Ordering is ABI: packages first (in element order), then all module
  // instances (in BFS elaboration order). slot_table.size() ==
  // num_design_slots.
  std::vector<TypeId> slot_table;
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
};

}  // namespace lyra::mir
