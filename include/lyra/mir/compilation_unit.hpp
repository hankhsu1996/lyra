#pragma once

#include <vector>

#include "lyra/mir/class_decl.hpp"
#include "lyra/mir/class_decl_id.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::mir {

class CompilationUnit {
 public:
  CompilationUnit() = default;

  [[nodiscard]] auto Types() const -> const std::vector<Type>&;
  [[nodiscard]] auto GetType(TypeId id) const -> const Type&;
  auto AddType(TypeData data) -> TypeId;

  [[nodiscard]] auto Classes() const -> const std::vector<ClassDecl>&;
  [[nodiscard]] auto GetClass(ClassDeclId id) const -> const ClassDecl&;
  auto GetClass(ClassDeclId id) -> ClassDecl&;
  auto AddClass(ClassDecl cls) -> ClassDeclId;

 private:
  std::vector<Type> types_;
  std::vector<ClassDecl> classes_;
};

}  // namespace lyra::mir
