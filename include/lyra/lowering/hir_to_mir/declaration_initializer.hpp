#pragma once

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/static_var_binding.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"

namespace lyra::lowering::hir_to_mir {

// Brings one body's static-lifetime declaration up (LRM 6.21) in the two phases
// `StorageBringUp` names.
//
// The initializer expression is lowered in the value frame's context -- its
// arena, its bindings, its `self` -- so the lowered output carries no arena
// affinity from the body it was written in. It lands as an assignment to the
// storage the binding designates, which for an observable-typed binding is the
// storage its cell stands for.
auto IntegrateStaticInitializer(
    ProcessLowerer& process, const hir::ProceduralBody& body,
    const StorageBringUp& bring_up, const StaticVarBinding& binding)
    -> diag::Result<void>;

}  // namespace lyra::lowering::hir_to_mir
