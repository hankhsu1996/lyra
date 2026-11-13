#pragma once

#include "lyra/hir/procedural_body.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/static_cell_binding.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"

namespace lyra::lowering::hir_to_mir {

// Brings up a cell one body keeps past an activation in the phases
// `StorageBringUp` names: a statement's assertion control, described as it is
// built and registered for the control tasks to select (LRM 20.11).
void BringUpStaticCell(
    ProcessLowerer& process, const hir::ProceduralBody& body,
    const StorageBringUp& bring_up, const StaticCellBinding& binding);

}  // namespace lyra::lowering::hir_to_mir
