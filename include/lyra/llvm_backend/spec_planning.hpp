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
}

namespace lyra::lowering::mir_to_llvm {

// All specialization planning products, computed once before compilation.
struct SpecPlan {
  std::vector<SpecCompilationUnit> units;
  std::vector<SpecSlotInfo> slot_infos;
  std::vector<CompiledModuleSpecInput> inputs;
  std::vector<ConnectionNotificationMask> connection_notification_masks;
};

// Build the complete specialization plan from design topology and layout.
// Returns a self-contained plan with all internal pointers wired up.
auto BuildSpecPlan(
    const mir::Design& design, const Layout& layout,
    std::span<const LayoutModulePlan> module_plans,
    const lowering::BodyOriginProvenance* origin_provenance) -> SpecPlan;

}  // namespace lyra::lowering::mir_to_llvm
