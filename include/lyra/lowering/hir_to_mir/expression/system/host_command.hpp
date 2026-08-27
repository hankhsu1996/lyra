#pragma once

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"

namespace lyra::lowering::hir_to_mir {

// Lower `$system` (LRM 20.17.1) into a `runtime.RunHostCommand(...)` call. The
// commanded form carries the command line as an SV `string`; the no-argument
// form carries nothing and reaches the host with the null command. Both yield
// the host's answer as an SV `int`, which a call written as a statement
// discards like any other unused value.
auto LowerHostCommandSystemSubroutineCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call)
    -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
