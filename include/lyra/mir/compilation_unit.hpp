#pragma once

#include <vector>

#include "lyra/mir/class_decl.hpp"
#include "lyra/mir/class_decl_id.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::mir {

struct CompilationUnit {
  std::vector<Type> types;
  std::vector<ClassDecl> classes;

  [[nodiscard]] auto GetType(TypeId id) const -> const Type& {
    return types.at(id.value);
  }
  [[nodiscard]] auto GetClass(ClassDeclId id) const -> const ClassDecl& {
    return classes.at(id.value);
  }
};

}  // namespace lyra::mir
