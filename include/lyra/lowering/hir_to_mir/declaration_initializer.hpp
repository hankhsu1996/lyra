#pragma once

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/static_var_binding.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"

namespace lyra::lowering::hir_to_mir {

// Brings one body's static-lifetime declaration up (LRM 6.21) in the two phases
// every such cell takes: `install_frame` receives the cell's declared
// representation and default, and `value_frame` the LRM 10.5 value
// initializer. A declaration scope that brings both up in one body passes one
// frame for both; a package keeps them apart, so that every cell in the design
// is installed before any initializer reads one.
//
// The initializer expression is lowered in the value frame's context -- its
// arena, its bindings, its `self` -- so the lowered output carries no arena
// affinity from the body it was written in. It lands as an assignment to the
// storage the binding designates, which for an observable-typed binding is the
// storage its cell stands for.
auto IntegrateStaticInitializer(
    ProcessLowerer& process, const hir::ProceduralBody& body,
    const WalkFrame& install_frame, const WalkFrame& value_frame,
    const StaticVarBinding& binding) -> diag::Result<void>;

}  // namespace lyra::lowering::hir_to_mir
