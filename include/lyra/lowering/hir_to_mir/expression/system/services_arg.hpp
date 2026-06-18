#pragma once

#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr_id.hpp"

namespace lyra::lowering::hir_to_mir {

// Builds the `self.Services()` argument: a `ProceduralVarRef` to `self`
// (mir.md invariant 11) wrapped in the `Services` scope-method call. Every
// runtime-effect generic call threads its result as the engine handle
// (docs/decisions/runtime-effects-as-generic-calls.md).
auto BuildServicesArg(const ProcessLowerer& process, const WalkFrame& frame)
    -> mir::ExprId;

}  // namespace lyra::lowering::hir_to_mir
