#pragma once

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

// Lower `$timeformat` (LRM 20.4.3) into a generic `CallExpr`. The four-argument
// form passes its lowered argument expressions to the set entry (the runtime
// narrows them); the no-argument form carries only `self.Services()` and
// selects the reset entry, which restores the design-global default the runtime
// resolves. Render picks set vs reset from the argument count.
auto LowerTimeFormatSystemSubroutineCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call,
    support::SystemSubroutineId id, diag::SourceSpan span)
    -> diag::Result<mir::Expr>;

// Lower `$printtimescale` (LRM 20.4.2, no-argument form) into a generic
// `CallExpr`. The enclosing scope's name, time unit, and precision are all
// resolved at lowering and passed as ordinary value arguments, so the runtime
// entry reads stated facts rather than the backend splicing scope constants.
auto LowerPrintTimescaleSystemSubroutineCall(
    const ProcessLowerer& process, const WalkFrame& frame,
    support::SystemSubroutineId id) -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
