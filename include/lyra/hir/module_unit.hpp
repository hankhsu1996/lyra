#pragma once

#include <string>
#include <vector>

#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/type.hpp"

namespace lyra::hir {

class ModuleUnit {
 public:
  explicit ModuleUnit(std::string name);
  ~ModuleUnit();
  ModuleUnit(ModuleUnit&&) noexcept;
  auto operator=(ModuleUnit&&) noexcept -> ModuleUnit&;
  ModuleUnit(const ModuleUnit&) = delete;
  auto operator=(const ModuleUnit&) -> ModuleUnit& = delete;

  [[nodiscard]] auto Name() const -> const std::string&;

  [[nodiscard]] auto Types() const -> const std::vector<Type>&;
  [[nodiscard]] auto GetType(TypeId id) const -> const Type&;
  auto AddType(TypeData data) -> TypeId;

  [[nodiscard]] auto RootScope() const -> const StructuralScope&;
  auto RootScope() -> StructuralScope&;

 private:
  std::string name_;
  std::vector<Type> types_;
  StructuralScope root_scope_;
};

}  // namespace lyra::hir
