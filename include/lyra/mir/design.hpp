#pragma once

#include <cstddef>
#include <variant>
#include <vector>

#include "lyra/mir/handle.hpp"
#include "lyra/mir/module.hpp"
#include "lyra/mir/package.hpp"

namespace lyra::mir {

using DesignElement = std::variant<Module, Package>;

struct Design {
  std::vector<DesignElement> elements;
  size_t num_design_slots = 0;
  std::vector<ProcessId> init_processes;
};

}  // namespace lyra::mir
