#pragma once

#include <cstddef>
#include <variant>
#include <vector>

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
};

}  // namespace lyra::mir
