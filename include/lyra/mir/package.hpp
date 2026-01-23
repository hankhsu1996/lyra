#pragma once

#include <vector>

#include "lyra/mir/handle.hpp"

namespace lyra::mir {

struct Package {
  std::vector<FunctionId> functions;
};

}  // namespace lyra::mir
