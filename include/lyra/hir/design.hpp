#pragma once

#include <vector>

#include "lyra/hir/module.hpp"

namespace lyra::hir {

struct Design {
  std::vector<Module> modules;
};

}  // namespace lyra::hir
