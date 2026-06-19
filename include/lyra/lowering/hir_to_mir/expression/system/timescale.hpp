#pragma once

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

// Lower `$timeformat` (LRM 20.4.3) into a void `mir::RuntimeCallExpr` carrying
// a `RuntimeSetTimeFormatCall`. The four-argument form lowers its arguments as
// expressions (the runtime narrows them); the no-argument form restores the
// defaults.
auto LowerTimeFormatSystemSubroutineCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call,
    diag::SourceSpan span) -> diag::Result<mir::Expr>;

// Lower `$printtimescale` (LRM 20.4.2, no-argument form) into a generic
// `CallExpr`. The enclosing scope's name, time unit, and precision are all
// resolved at lowering and passed as ordinary value arguments, so the runtime
// entry reads stated facts rather than the backend splicing scope constants.
auto LowerPrintTimescaleSystemSubroutineCall(
    const ProcessLowerer& process, const WalkFrame& frame,
    support::SystemSubroutineId id) -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
