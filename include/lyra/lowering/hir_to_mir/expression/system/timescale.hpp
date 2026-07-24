#pragma once

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"

namespace lyra::lowering::hir_to_mir {

// Lower `$timeformat` (LRM 20.4.3) into a `runtime.SetTimeFormat(...)` or
// `runtime.ResetTimeFormat()` call. The four-argument form lowers each
// operand into a value expression and routes to the set method; the
// no-argument form selects the reset method, which restores the LRM Table
// 20-3 defaults the runtime resolves.
auto LowerTimeFormatSystemSubroutineCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call,
    diag::SourceSpan span) -> diag::Result<mir::Expr>;

// Lower `$printtimescale` (LRM 20.4.2, no-argument form) into a
// `runtime.Files().Writeln(stdout_fd, text)` call. The scope's name, time
// unit, and precision are all known at lowering time, so the message text is
// formatted into a string literal here -- the runtime only sees the same sink
// write that `$display` uses.
auto LowerPrintTimescaleSystemSubroutineCall(
    const ProcessLowerer& process, const WalkFrame& frame)
    -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
