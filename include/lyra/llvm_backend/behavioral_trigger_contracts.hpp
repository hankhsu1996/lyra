#pragma once

#include <span>

#include "lyra/llvm_backend/layout/layout.hpp"

namespace lyra::mir {
class Arena;
struct ConstructionInput;
struct Design;
}  // namespace lyra::mir

namespace lyra::lowering::mir_to_llvm {

// Populate all behavioral dirty-trigger contracts on Layout:
//   1. Body-relative behavioral trigger bitmaps (per-body
//   slot_has_behavioral_trigger)
//   2. Design-global behavioral trigger bitmap
//   (slot_has_design_behavioral_trigger)
//
// construction: used to resolve ext-ref trigger targets to body-local
// slots on the target body for cross-body behavioral dirty marking.
//
// Must complete before codegen reads RequiresDirtyPropagation.
void PopulateBehavioralTriggerContracts(
    std::span<const LayoutModulePlan> module_plans,
    std::span<const std::span<const mir::ProcessId>> module_body_processes,
    const mir::Design& design, const mir::Arena& design_arena,
    const mir::ConstructionInput& construction, Layout& layout);

}  // namespace lyra::lowering::mir_to_llvm
