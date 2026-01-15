#pragma once

#include <vector>

#include "lyra/mir/handle.hpp"

namespace lyra::mir {

struct Module {
  std::vector<ProcessId> processes;
  std::vector<FunctionId> functions;
};

}  // namespace lyra::mir
