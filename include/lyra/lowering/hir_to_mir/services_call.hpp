#pragma once

#include "lyra/lowering/hir_to_mir/module_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"

namespace lyra::lowering::hir_to_mir {

// Builds the engine-handle expression `self.Services()`: a `Services`
// scope-method call whose receiver is the body's self read, interned as a
// child. Returns the call node detached; the caller interns it. Runtime-effect
// generic calls -- $finish, the $time family, named-event trigger / triggered
// -- thread it as the engine handle. It needs only the module and the walk
// frame, not the pass class, so it serves the process body and structural-scope
// (continuous-assign) call paths identically.
auto BuildServicesCallExpr(const ModuleLowerer& module, const WalkFrame& frame)
    -> mir::Expr;

// Builds the file-IO broker expression `services.Files()`: a `Files` method
// call on the engine handle, typed as the unit's `files` builtin. Returns the
// outer call detached; the caller interns it. The inner services call is
// interned into `frame.current_block` as a child. LRM 21.3 file-IO ops and
// LRM 21.2.1 / 21.3.1 sink writes thread the resulting handle as the receiver
// of their FileTable methods.
auto BuildFilesCallExpr(const ModuleLowerer& module, const WalkFrame& frame)
    -> mir::Expr;

}  // namespace lyra::lowering::hir_to_mir
