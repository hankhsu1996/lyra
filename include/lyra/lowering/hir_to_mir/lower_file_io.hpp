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

// Lower a file IO system subroutine call ($fopen / $fclose / $fgetc /
// $ungetc / $fseek / $rewind / $ftell / $feof / $fflush) into a mir::Expr
// carrying a mir::RuntimeCallExpr with the kind-specific payload.
// Output-arg tasks ($fgets / $fread / $ferror) reach this path only when
// nested inside a larger expression and return diag::Unsupported -- the
// statement-position desugaring runs upstream via
// LowerFileIOSystemSubroutineCallStmt.
auto LowerFileIOSystemSubroutineCall(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::ProceduralBody& hir_proc, const hir::CallExpr& call,
    const support::SystemSubroutineDesc& desc,
    const support::FileIOSystemSubroutineInfo& info, diag::SourceSpan span)
    -> diag::Result<mir::Expr>;

// LRM 13.5 copy-out at the statement boundary for the three file IO tasks
// that write through an output argument. Synthesizes the BlockStmt that UDF
// `output` args use: temp local of the dest's type, call with that temp in
// the output slot, then a writeback assign from temp to the user's actual
// lvalue. The optional `assign_target` carries the LHS for the
// `lhs = $fgets(...)` shape; nullopt means a bare-call statement. The
// result_type is the call's int32 return slot.
auto LowerFileIOSystemSubroutineCallStmt(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    ProcessLoweringState& proc_state, const hir::ProceduralBody& hir_proc,
    const hir::Stmt& stmt, const hir::CallExpr& call,
    const support::FileIOSystemSubroutineInfo& info,
    std::optional<hir::ExprId> assign_target, mir::TypeId result_type)
    -> diag::Result<mir::Stmt>;

}  // namespace lyra::lowering::hir_to_mir
