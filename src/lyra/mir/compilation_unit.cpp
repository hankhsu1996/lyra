#include "lyra/mir/compilation_unit.hpp"

#include <cstdint>
#include <utility>

namespace lyra::mir {

auto CompilationUnit::Classes() const -> const std::vector<ClassDecl>& {
  return classes_;
}

auto CompilationUnit::GetClass(ClassId id) const -> const ClassDecl& {
  return classes_.at(id.value);
}

auto CompilationUnit::GetClass(ClassId id) -> ClassDecl& {
  return classes_.at(id.value);
}

auto CompilationUnit::AddClass(ClassDecl cls) -> ClassId {
  const ClassId id{static_cast<std::uint32_t>(classes_.size())};
  classes_.push_back(std::move(cls));
  return id;
}

}  // namespace lyra::mir
