#pragma once

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerStmt(ProcessLowerer& process, WalkFrame frame, const hir::Stmt& stmt)
    -> diag::Result<mir::Stmt>;

// Lowers a single HIR stmt into its own fresh ProceduralScope: opens a child
// procedural scope, descends through `frame.WithProceduralScope(...).Deeper()`
// so procvar refs see the correct hop count, and packages the result as the
// new scope's sole root stmt. Used wherever a control-flow node needs a body
// or branch scope (for, while, repeat, do-while, forever, if branches, case
// items).
auto LowerStmtIntoChildScope(
    ProcessLowerer& process, WalkFrame frame, hir::StmtId hir_stmt_id)
    -> diag::Result<mir::ProceduralScope>;

}  // namespace lyra::lowering::hir_to_mir
