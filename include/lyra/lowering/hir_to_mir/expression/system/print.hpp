#pragma once

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

// Lower a print-like system subroutine ($display / $write / $fdisplay /
// $fwrite / $strobe family) into a generic `CallExpr`. Returns a user
// diagnostic for unsupported shapes (`%m`, malformed format string, ...).
auto LowerPrintSystemSubroutineCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call,
    const support::PrintSystemSubroutineInfo& print, diag::SourceSpan span)
    -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
