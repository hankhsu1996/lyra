#include "lyra/mir/compilation_unit.hpp"

#include <cstdint>
#include <utility>
#include <vector>

#include "lyra/mir/class_decl.hpp"
#include "lyra/mir/class_decl_id.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::mir {

auto CompilationUnit::Types() const -> const std::vector<Type>& {
  return types_;
}

auto CompilationUnit::GetType(TypeId id) const -> const Type& {
  return types_.at(id.value);
}

auto CompilationUnit::AddType(TypeData data) -> TypeId {
  const TypeId id{static_cast<std::uint32_t>(types_.size())};
  types_.push_back(Type{.data = std::move(data)});
  return id;
}

auto CompilationUnit::Classes() const -> const std::vector<ClassDecl>& {
  return classes_;
}

auto CompilationUnit::GetClass(ClassDeclId id) const -> const ClassDecl& {
  return classes_.at(id.value);
}

auto CompilationUnit::GetClass(ClassDeclId id) -> ClassDecl& {
  return classes_.at(id.value);
}

auto CompilationUnit::AddClass(ClassDecl cls) -> ClassDeclId {
  const ClassDeclId id{static_cast<std::uint32_t>(classes_.size())};
  classes_.push_back(std::move(cls));
  return id;
}

}  // namespace lyra::mir
