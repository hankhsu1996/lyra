#pragma once

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

// Lower a severity task, $info / $warning / $error / $fatal (LRM 20.10): its
// message is formatted as the print family formats one and reported at the
// task's severity. A $fatal then ends the run, and that call is the returned
// expression; for the others it is the report.
auto LowerDiagnosticSystemSubroutineCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call,
    const support::DiagnosticSystemSubroutineInfo& info, diag::SourceSpan span)
    -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
