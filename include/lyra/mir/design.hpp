#pragma once

#include <variant>
#include <vector>

#include "lyra/mir/module.hpp"
#include "lyra/mir/package.hpp"

namespace lyra::mir {

using DesignElement = std::variant<Module, Package>;

struct Design {
  std::vector<DesignElement> elements;
};

}  // namespace lyra::mir
