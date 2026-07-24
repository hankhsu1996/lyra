#pragma once

#include "lyra/diag/diagnostic.hpp"
#include "lyra/lowering/hir_to_mir/expression/expr_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

// Lower a simulation-time read ($time / $stime / $realtime, LRM 20.3) into a
// generic `mir::CallExpr`: the engine handle (`self.Services()`) followed by
// the calling scope's time-unit power (LRM 3.14.2), which the runtime uses to
// scale the design-global tick back to that unit. The result type follows the
// kind: 64-bit time, 32-bit int, or real. A time read is a pure value query
// with no statement sequencing, so it is legal in a continuous assignment as
// well as in procedural code and one template serves both pass classes.
template <ExprLowerer Lowerer>
auto LowerTimeSystemSubroutineCall(
    const Lowerer& lowerer, const WalkFrame& frame,
    const support::TimeSystemSubroutineInfo& info) -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
