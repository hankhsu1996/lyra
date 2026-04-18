#pragma once

#include <vector>

#include "lyra/common/local_slot_id.hpp"
#include "lyra/common/type.hpp"
#include "lyra/mir/routine.hpp"

namespace lyra::mir {
struct ModuleBody;
struct Function;
class Arena;
}  // namespace lyra::mir

namespace lyra::lowering::hir_to_mir {

// Extract the parent-local storage read set from a body-local writeback
// function. Returned slots define the dependency set for installable
// computation trigger registration. Empty result means the expression
// has no runtime dependencies (constant expression).
auto CollectParentLocalReadSlotsFromExprFunction(
    const mir::Function& func, const mir::Arena& arena)
    -> std::vector<common::LocalSlotId>;

// Build installable computation templates for every reactive
// parent->child binding in the body. One body-local writeback function
// has already been produced per binding at recipe creation; this step
// extracts the dependency set from that function and packages the
// callable, deps, and diagnostic child identity into an
// InstallableComputationTemplate. The function is void-returning and
// writes its child target through the ExternalRefId registered with
// the recipe. Does NOT create any processes.
void BuildInstallableComputations(mir::ModuleBody& body);

}  // namespace lyra::lowering::hir_to_mir
