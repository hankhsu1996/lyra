#pragma once

#include <vector>

#include "lyra/mir/structural_scope.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::mir {

struct CompilationUnit {
  std::vector<Type> types;
  StructuralScope structural_scope;

  [[nodiscard]] auto GetType(TypeId id) const -> const Type& {
    return types.at(id.value);
  }
};

}  // namespace lyra::mir
