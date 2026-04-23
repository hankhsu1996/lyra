#pragma once

#include "lyra/hir/module_unit.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/mir/module_unit.hpp"
#include "lyra/mir/process.hpp"

namespace lyra::lowering::hir_to_mir {

class UnitLoweringFacts {
 public:
  UnitLoweringFacts(const hir::ModuleUnit& hir_unit, mir::ModuleUnit& mir_unit)
      : hir_unit_(&hir_unit), mir_unit_(&mir_unit) {
  }

  [[nodiscard]] auto HirUnit() const -> const hir::ModuleUnit& {
    return *hir_unit_;
  }

  [[nodiscard]] auto MirUnit() const -> mir::ModuleUnit& {
    return *mir_unit_;
  }

 private:
  const hir::ModuleUnit* hir_unit_;
  mir::ModuleUnit* mir_unit_;
};

class ProcessLoweringFacts {
 public:
  ProcessLoweringFacts(
      const UnitLoweringFacts& unit, const hir::Process& hir_process,
      mir::Process& mir_process)
      : unit_(&unit), hir_process_(&hir_process), mir_process_(&mir_process) {
  }

  [[nodiscard]] auto Unit() const -> const UnitLoweringFacts& {
    return *unit_;
  }

  [[nodiscard]] auto HirProcess() const -> const hir::Process& {
    return *hir_process_;
  }

  [[nodiscard]] auto MirProcess() const -> mir::Process& {
    return *mir_process_;
  }

 private:
  const UnitLoweringFacts* unit_;
  const hir::Process* hir_process_;
  mir::Process* mir_process_;
};

}  // namespace lyra::lowering::hir_to_mir
