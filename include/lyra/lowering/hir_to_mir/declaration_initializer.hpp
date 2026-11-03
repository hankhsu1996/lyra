#pragma once

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/static_cell_binding.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"

namespace lyra::lowering::hir_to_mir {

// Brings one body's static-lifetime cell up in the two phases `StorageBringUp`
// names: a variable's initial value (LRM 6.21), or a statement's assertion
// control, registered for the control tasks to select (LRM 20.11).
//
// The initializer expression is lowered in the value frame's context -- its
// arena, its bindings, its `self` -- so the lowered output carries no arena
// affinity from the body it was written in. It lands as an assignment to the
// storage the binding designates, which for an observable-typed binding is the
// storage its cell stands for.
auto IntegrateStaticInitializer(
    ProcessLowerer& process, const hir::ProceduralBody& body,
    const StorageBringUp& bring_up, const StaticCellBinding& binding)
    -> diag::Result<void>;

}  // namespace lyra::lowering::hir_to_mir
