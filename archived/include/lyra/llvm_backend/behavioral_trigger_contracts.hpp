#pragma once

#include <span>

#include "lyra/llvm_backend/layout/layout.hpp"

namespace lyra::mir {
class Arena;
struct ConstructionInput;
struct Design;
}  // namespace lyra::mir

namespace lyra::lowering::mir_to_llvm {

// Populate behavioral dirty-trigger contracts on Layout:
//   - Body-relative behavioral trigger bitmaps (slot_has_behavioral_trigger)
//   - Cross-body behavioral trigger bitmaps
//     (slot_has_cross_body_behavioral_trigger)
//
// construction: used to resolve ext-ref trigger targets to body-local
// slots on the target body for cross-body behavioral dirty marking.
//
// Must complete before codegen reads RequiresDirtyPropagation.
void PopulateBehavioralTriggerContracts(
    std::span<const LayoutModulePlan> module_plans, const mir::Design& design,
    const mir::Arena& design_arena, const mir::ConstructionInput& construction,
    Layout& layout);

}  // namespace lyra::lowering::mir_to_llvm
