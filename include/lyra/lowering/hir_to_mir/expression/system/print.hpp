#pragma once

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

// Lower a print-like system subroutine call into a `mir::Expr`. $display /
// $write / $fdisplay / $fwrite become a generic `CallExpr` whose arguments are
// the engine handle, an optional descriptor, and a constructed `PrintItem`
// array; $strobe-family calls defer the same print through a postponed submit.
// Returns a user diagnostic for unsupported shapes (%m, malformed format
// string, etc.).
auto LowerPrintSystemSubroutineCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call,
    support::SystemSubroutineId id,
    const support::PrintSystemSubroutineInfo& print, diag::SourceSpan span)
    -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
