#pragma once

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

// Lower a severity task that leaves the run going, $info / $warning / $error
// (LRM 20.10): its message is formatted as the print family formats one and
// reported at the task's severity, and the report is the returned expression.
auto LowerDiagnosticSystemSubroutineCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call,
    const support::DiagnosticSystemSubroutineInfo& info)
    -> diag::Result<mir::Expr>;

// Lower $fatal (LRM 20.10): the report is a statement of its own, and the
// implicit $finish after it is the returned expression.
auto LowerFatalSystemSubroutineCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call)
    -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
