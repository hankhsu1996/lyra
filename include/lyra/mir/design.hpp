#pragma once

#include <cstddef>
#include <cstdint>
#include <variant>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "lyra/common/type.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/module.hpp"
#include "lyra/mir/package.hpp"

namespace lyra::mir {

using DesignElement = std::variant<Module, Package>;

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

  // Alias map for output/inout ports.
  // Key: child port's SlotId (must be kDesign root with no projections)
  // Value: parent's PlaceId (may have projections, must resolve to kDesign)
  // Currently consumed by LLVM backend only; MIR interpreter does not resolve.
  absl::flat_hash_map<SlotId, PlaceId> alias_map;
};

}  // namespace lyra::mir
