#pragma once

#include <vector>

#include "lyra/mir/handle.hpp"

namespace lyra::mir {

struct Module {
  std::vector<ProcessId> processes;
  std::vector<FunctionId> functions;
  size_t num_module_slots = 0;
};

}  // namespace lyra::mir
