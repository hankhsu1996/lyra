#pragma once

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

// Lower a termination system subroutine call ($finish) into a `mir::Expr`
// carrying a `mir::RuntimeCallExpr` with a `RuntimeFinishCall` payload.
// Resolves the optional level argument from the call's literal integer arg
// (falling back to the descriptor's default_level when omitted). Returns a
// user diagnostic if the level arg is non-literal or out of range.
auto LowerFinishSystemSubroutineCall(
    const UnitLoweringState& unit_state, const hir::ProceduralBody& hir_proc,
    const hir::CallExpr& call, const support::SystemSubroutineDesc& desc,
    const support::TerminationSystemSubroutineInfo& info, diag::SourceSpan span)
    -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
