#pragma once

#include <variant>
#include <vector>

#include "lyra/hir/module.hpp"
#include "lyra/hir/module_body.hpp"
#include "lyra/hir/package.hpp"

namespace lyra::hir {

using DesignElement = std::variant<Module, Package>;

struct Design {
  std::vector<DesignElement> elements;
  // Specialization-owned shared module bodies, indexed by ModuleBodyId.
  // One body per specialization group.
  std::vector<ModuleBody> module_bodies;
};

}  // namespace lyra::hir
