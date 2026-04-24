#pragma once

#include <vector>

#include "lyra/mir/class_decl.hpp"

namespace lyra::mir {

class CompilationUnit {
 public:
  CompilationUnit() = default;

  [[nodiscard]] auto Classes() const -> const std::vector<ClassDecl>&;
  [[nodiscard]] auto GetClass(ClassId id) const -> const ClassDecl&;
  auto GetClass(ClassId id) -> ClassDecl&;
  auto AddClass(ClassDecl cls) -> ClassId;

 private:
  std::vector<ClassDecl> classes_;
};

}  // namespace lyra::mir
