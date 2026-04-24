#include "lyra/hir/module_unit.hpp"

#include <utility>

namespace lyra::hir {

ModuleUnit::ModuleUnit(std::string name) : name_(std::move(name)) {
}

ModuleUnit::~ModuleUnit() = default;
ModuleUnit::ModuleUnit(ModuleUnit&&) noexcept = default;
auto ModuleUnit::operator=(ModuleUnit&&) noexcept -> ModuleUnit& = default;

auto ModuleUnit::Name() const -> const std::string& {
  return name_;
}

auto ModuleUnit::Types() const -> const std::vector<Type>& {
  return types_;
}

auto ModuleUnit::GetType(TypeId id) const -> const Type& {
  return types_.at(id.value);
}

auto ModuleUnit::AddType(TypeData data) -> TypeId {
  const TypeId id{static_cast<std::uint32_t>(types_.size())};
  types_.push_back(Type{.data = std::move(data)});
  return id;
}

auto ModuleUnit::RootScope() const -> const StructuralScope& {
  return root_scope_;
}

auto ModuleUnit::RootScope() -> StructuralScope& {
  return root_scope_;
}

}  // namespace lyra::hir
