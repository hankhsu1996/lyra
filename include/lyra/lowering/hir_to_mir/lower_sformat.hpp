#pragma once

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

// LRM 21.3.3 expression-position entry. Only `$sformatf` reaches here
// legitimately (a function returning a `value::String`); `$sformat` /
// `$swrite*` are tasks and slang rejects them as expressions, so a call
// arriving with `info.has_output_arg == true` is an upstream invariant
// violation and raises `InternalError`.
auto LowerSFormatSystemSubroutineCall(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::ProceduralBody& hir_proc, const hir::CallExpr& call,
    const support::SFormatSystemSubroutineInfo& info, diag::SourceSpan span)
    -> diag::Result<mir::Expr>;

// LRM 21.3.3 statement-position entry. Two shapes:
//   - `$sformat` / `$swrite*` (has_output_arg == true): build
//       AssignStmt { lhs = out_var, rhs = LyraSFormat(items) }.
//   - `$sformatf(...)` as a discard statement (has_output_arg == false):
//       build ExprStmt { expr = LyraSFormat(items) }.
// No copy-out wrapper -- the output_var is a single string-typed lvalue
// fully overwritten by the call result, so the standard AssignStmt
// covers LRM 13.5 lvalue-binding order naturally.
auto LowerSFormatSystemSubroutineCallStmt(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::ProceduralBody& hir_proc, const hir::Stmt& stmt,
    diag::SourceSpan span, const hir::CallExpr& call,
    const support::SystemSubroutineDesc& desc,
    const support::SFormatSystemSubroutineInfo& info)
    -> diag::Result<mir::Stmt>;

}  // namespace lyra::lowering::hir_to_mir
