#pragma once

// Lowering of block-shaped statements: `empty` and `begin...end` blocks
// (LRM 9.3). Also exposes `LowerStmtIntoChildScope`, the helper every
// control-flow statement uses to package a body / branch as its own fresh
// `mir::Block`.

#include <optional>
#include <string>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/field.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerEmptyStmt(std::optional<std::string> label)
    -> diag::Result<mir::Stmt>;

// Builds the region that consumes a `disable` of `target` (LRM 9.6.2) around an
// already-lowered `body`. The region brackets the body with the target's extent
// -- so a `disable` reaches every execution inside it, and a check finds the
// target among the ones that execution is inside -- and binds the effect
// leaving the body, ending it here when it names this target, so execution
// continues past the region, and raising it again for the region that does name
// its target. `target` is a field of the class enclosing this body; a named
// block, a named fork, and a task each build one alike.
auto BuildCancellableRegion(
    ProcessLowerer& process, const WalkFrame& frame, mir::Block&& body,
    mir::FieldId target) -> mir::TryStmt;

auto LowerBlockStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::BlockStmt& b) -> diag::Result<mir::Stmt>;

// Lowers `disable <named block or task>` (LRM 9.6.2) to the one call that
// carries the whole statement: it invalidates the named target, wakes what is
// blocked inside it, and leaves the disabling execution when that execution is
// inside the target too. Where any affected execution lands is not decided
// here.
auto LowerDisableStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::DisableStmt& d) -> diag::Result<mir::Stmt>;

// Lowers a single HIR stmt into its own fresh `mir::Block`: opens a child
// block, descends through `frame.WithBlock(...)`, and packages the result as
// the new block's sole root stmt. Used wherever a control-flow node needs a
// body or branch scope (for, while, repeat, do-while, forever, if branches,
// case items).
auto LowerStmtIntoChildScope(
    ProcessLowerer& process, WalkFrame frame, hir::StmtId hir_stmt_id)
    -> diag::Result<mir::Block>;

}  // namespace lyra::lowering::hir_to_mir
