#pragma once

#include "lyra/hir/module_unit.hpp"
#include "lyra/hir/process.hpp"

namespace lyra::lowering::hir_to_mir {

class UnitLoweringFacts {
 public:
  explicit UnitLoweringFacts(const hir::ModuleUnit& hir_unit)
      : hir_unit_(&hir_unit) {
  }

  [[nodiscard]] auto HirUnit() const -> const hir::ModuleUnit& {
    return *hir_unit_;
  }

 private:
  const hir::ModuleUnit* hir_unit_;
};

class ProcessLoweringFacts {
 public:
  ProcessLoweringFacts(
      const UnitLoweringFacts& unit, const hir::Process& hir_process)
      : unit_(&unit), hir_process_(&hir_process) {
  }

  [[nodiscard]] auto Unit() const -> const UnitLoweringFacts& {
    return *unit_;
  }

  [[nodiscard]] auto HirProcess() const -> const hir::Process& {
    return *hir_process_;
  }

 private:
  const UnitLoweringFacts* unit_;
  const hir::Process* hir_process_;
};

}  // namespace lyra::lowering::hir_to_mir
