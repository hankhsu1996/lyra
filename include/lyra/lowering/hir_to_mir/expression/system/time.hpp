#pragma once

#include "lyra/diag/diagnostic.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

// Lower a simulation-time read ($time / $stime / $realtime, LRM 20.3) into a
// generic `mir::CallExpr`: the engine handle (`self.Services()`) followed by
// the calling scope's time-unit power (LRM 3.14.2), which the runtime uses to
// scale the design-global tick back to that unit. The result type follows the
// kind: 64-bit time, 32-bit int, or real.
auto LowerTimeSystemSubroutineCall(
    const ProcessLowerer& process, const WalkFrame& frame,
    support::SystemSubroutineId id,
    const support::TimeSystemSubroutineInfo& info) -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
