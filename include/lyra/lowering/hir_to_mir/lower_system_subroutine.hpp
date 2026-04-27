#pragma once

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

// Lower an expression-statement system-subroutine call by dispatching on the
// subroutine's semantic kind. Print semantics route to print lowering;
// unsupported semantic kinds produce a user diagnostic.
auto LowerSystemSubroutineStmt(
    const UnitLoweringState& unit_state, const ProcessLoweringState& proc_state,
    const hir::Process& hir_proc, const BodyLoweringState& body_state,
    const hir::CallExpr& call, const hir::SystemSubroutineRef& ref,
    diag::SourceSpan span) -> diag::Result<mir::StmtData>;

}  // namespace lyra::lowering::hir_to_mir
