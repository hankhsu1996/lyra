#pragma once

#include <string>
#include <vector>

#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/type.hpp"

namespace lyra::hir {

struct ModuleUnit {
  std::string name;
  std::vector<Type> types;
  StructuralScope root_scope;

  [[nodiscard]] auto GetType(TypeId id) const -> const Type& {
    return types.at(id.value);
  }
};

}  // namespace lyra::hir
