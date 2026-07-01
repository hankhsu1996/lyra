#pragma once

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/lowering/hir_to_mir/callable_storage_plan.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"

namespace lyra::lowering::hir_to_mir {

// Materializes one pending static initializer (LRM 13.3.1). The HIR
// initializer expression is lowered in the caller-chosen frame's context --
// its arena, its bindings, its `self` -- so the lowered output never carries
// arena affinity from the body-lowering pass that deferred it. The result
// lands as an assignment to the storage placement, wrapped in the
// observable cell protocol when the storage is observable-typed.
auto IntegratePendingStaticInitializer(
    ProcessLowerer& process, const hir::ProceduralBody& body,
    const WalkFrame& init_frame, const PendingStaticInitializer& pending)
    -> diag::Result<void>;

}  // namespace lyra::lowering::hir_to_mir
