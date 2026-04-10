#pragma once

#include <span>
#include <vector>

#include "lyra/llvm_backend/codegen_session.hpp"
#include "lyra/llvm_backend/layout/layout.hpp"

namespace lyra::lowering {
struct BodyOriginProvenance;
}

namespace lyra::mir {
struct Design;
struct DpiExportWrapperDesc;
}  // namespace lyra::mir

namespace lyra::lowering::mir_to_llvm {

// All specialization planning products, computed once before compilation.
struct SpecPlan {
  std::vector<SpecCompilationUnit> units;
  std::vector<SpecSlotInfo> slot_infos;
  std::vector<CompiledModuleSpecInput> inputs;
  std::vector<ConnectionNotificationMask> connection_notification_masks;
  // Per-body module export targets. Parallel to inputs.
  // Inputs carry spans into this storage.
  std::vector<std::vector<ModuleExportTarget>> module_export_targets;
};

// Build the complete specialization plan from design topology and layout.
// Returns a self-contained plan with all internal pointers wired up.
auto BuildSpecPlan(
    const mir::Design& design, const Layout& layout,
    std::span<const LayoutModulePlan> module_plans,
    const lowering::BodyOriginProvenance* origin_provenance,
    std::span<const mir::DpiExportWrapperDesc> dpi_export_wrappers) -> SpecPlan;

}  // namespace lyra::lowering::mir_to_llvm
