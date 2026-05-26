#pragma once

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerStmt(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::Process& hir_proc, const hir::Stmt& stmt)
    -> diag::Result<mir::Stmt>;

// Lowers a single HIR stmt into its own fresh ProceduralScope: pushes a
// ProceduralDepthGuard so procvar refs inside see the correct hop count,
// recursively invokes LowerStmt, and packages the result as the new scope's
// sole root stmt. Used wherever a control-flow node needs a body / branch
// scope (for, while, repeat, do-while, forever, if branches, case items).
auto LowerStmtIntoChildScope(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    ProcessLoweringState& proc_state, const hir::Process& hir_proc,
    hir::StmtId hir_stmt_id) -> diag::Result<mir::ProceduralScope>;

}  // namespace lyra::lowering::hir_to_mir
