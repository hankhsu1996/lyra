#pragma once

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

// LRM 21.3.4.3 `$sscanf` / `$fscanf`. The parse completes with the
// matched-conversion count, how far it advanced, and one value per conversion;
// the call is modelled as a block expression whose steps bind that completion,
// store each parsed value into the lvalue the source named for it, and yield
// the count. Each conversion hands the parse a prototype of the shape it reads
// into, which nothing else on the call states.
auto LowerScanSystemSubroutineCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call,
    const support::ScanSystemSubroutineInfo& info, diag::SourceSpan span)
    -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
