#pragma once

#include <vector>

#include "lyra/common/symbol_types.hpp"
#include "lyra/mir/handle.hpp"

namespace lyra::mir {

struct Module {
  SymbolId instance_sym;  // Instance symbol for mapping drive bindings
  std::vector<ProcessId> processes;
  std::vector<FunctionId> functions;
};

}  // namespace lyra::mir
