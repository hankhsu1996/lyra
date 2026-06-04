#pragma once

#include <optional>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

// Reject a $sscanf / $fscanf reached in expression position (e.g. inside an
// `if` or a non-blocking-assignment context). The scan family writes through
// output argument lvalues, which only lowers cleanly at the statement boundary
// where the temp + writeback BlockStmt can wrap the call (see
// LowerScanSystemSubroutineCallStmt, dispatched from lower_stmt.cpp); every
// other position is unsupported.
auto RejectScanCallInExprPosition(
    const support::SystemSubroutineDesc& desc, diag::SourceSpan span)
    -> diag::Result<mir::Expr>;

// Statement-position $sscanf desugaring (LRM 13.5 copy-out for every output
// argument). Builds:
//   { temp_i = init_i; ... ;
//     [lhs =] LyraSScanf(input, format, slots);
//     actual_i = temp_i; ... }
// The temps are copy-in initialized from each actual (LRM 21.3.4.3 leaves
// unmatched output args untouched, so copy-in lets the unconditional
// writeback round-trip to a no-op when the scanner skips a slot).
//
// `assign_target` carries the LHS for `lhs = $sscanf(...)`; `nullopt` is
// a bare-call statement. `result_type` is the call's int32 return slot.
auto LowerScanSystemSubroutineCallStmt(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    ProcessLoweringState& proc_state, const hir::ProceduralBody& hir_proc,
    const hir::Stmt& stmt, diag::SourceSpan span, const hir::CallExpr& call,
    const support::SystemSubroutineDesc& desc,
    const support::ScanSystemSubroutineInfo& info,
    std::optional<hir::ExprId> assign_target, mir::TypeId result_type)
    -> diag::Result<mir::Stmt>;

}  // namespace lyra::lowering::hir_to_mir
