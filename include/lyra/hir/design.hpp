#pragma once

#include <variant>
#include <vector>

#include "lyra/hir/module.hpp"
#include "lyra/hir/package.hpp"

namespace lyra::hir {

using DesignElement = std::variant<Module, Package>;

struct Design {
  std::vector<DesignElement> elements;
};

}  // namespace lyra::hir
