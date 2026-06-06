#pragma once

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/expr.hpp"

namespace lyra::lowering::hir_to_mir {

// Lower `$timeformat` (LRM 20.4.3) into a void `mir::RuntimeCallExpr` carrying
// a `RuntimeSetTimeFormatCall`. The four-argument form lowers its arguments as
// expressions (the runtime narrows them); the no-argument form restores the
// defaults.
auto LowerTimeFormatSystemSubroutineCall(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::ProceduralBody& hir_proc, const hir::CallExpr& call,
    diag::SourceSpan span) -> diag::Result<mir::Expr>;

// Lower `$printtimescale` (LRM 20.4.2, no-argument form) into a void
// `mir::RuntimeCallExpr` carrying a `RuntimePrintTimescaleCall` with the
// enclosing scope's name.
auto LowerPrintTimescaleSystemSubroutineCall(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state) -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
