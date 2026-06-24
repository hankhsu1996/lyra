#pragma once

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

// Lower $info / $warning / $error into the two-step composition the print
// family uses: `services.Format(items)` yields the formatted text, then
// `services.Diagnostic().EmitX(text)` emits at the severity selected by
// `info.builtin_fn` (LRM 20.10). Shares format-item parsing with the print
// path; the returned expression is the void Emit call.
auto LowerDiagnosticSystemSubroutineCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call,
    const support::DiagnosticSystemSubroutineInfo& info, diag::SourceSpan span)
    -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
