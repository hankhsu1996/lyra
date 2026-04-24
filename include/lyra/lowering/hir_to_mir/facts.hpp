#pragma once

#include "lyra/hir/module_unit.hpp"
#include "lyra/hir/structural_scope.hpp"

namespace lyra::lowering::hir_to_mir {

class UnitLoweringFacts {
 public:
  UnitLoweringFacts(
      const hir::ModuleUnit& hir, const hir::StructuralScope& root_scope)
      : hir_(&hir), root_scope_(&root_scope) {
  }

  [[nodiscard]] auto Hir() const -> const hir::ModuleUnit& {
    return *hir_;
  }

  [[nodiscard]] auto RootScope() const -> const hir::StructuralScope& {
    return *root_scope_;
  }

 private:
  const hir::ModuleUnit* hir_;
  const hir::StructuralScope* root_scope_;
};

}  // namespace lyra::lowering::hir_to_mir
