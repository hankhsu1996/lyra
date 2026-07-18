#pragma once

#include <string>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/continuous_assign.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/callable.hpp"

namespace lyra::lowering::hir_to_mir {

// Lowers a continuous assignment fully, including any Resolve- and
// Initialize-phase side effects the LHS's type demands. A variable-typed
// LHS produces a body coroutine that writes the cell directly every
// iteration (LRM 10.3.2). A resolved-net-typed LHS installs a driver-handle
// field on the enclosing class, attaches an independent driver at Resolve,
// seeds it at Initialize, and the body updates the driver every iteration;
// the net resolves its drivers, so several assignments to one net each
// install their own driver (LRM 6.5). The write protocol is picked by the
// LHS's MIR type; callers see one API for both cases. The three phase frames
// all address the same enclosing class; each carries its phase's target
// block.
auto LowerContinuousAssign(
    const StructuralScopeLowerer& lowerer, const WalkFrame& ctor_frame,
    const WalkFrame& resolve_frame, const WalkFrame& init_frame,
    std::string name, const hir::ContinuousAssign& src)
    -> diag::Result<mir::CallableDecl>;

}  // namespace lyra::lowering::hir_to_mir
