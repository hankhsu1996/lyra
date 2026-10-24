#pragma once

// Lowering of block-shaped statements: `empty` and `begin...end` blocks
// (LRM 9.3). Also exposes `LowerStmtIntoChildScope`, the helper every
// control-flow statement uses to package a body / branch as its own fresh
// `mir::Block`.

#include <optional>
#include <span>
#include <string>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/support/target_landing.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerEmptyStmt(std::optional<std::string> label)
    -> diag::Result<mir::Stmt>;

// Brackets an already-lowered `body` with the extent of `target` (LRM 9.6.2),
// so a `disable` reaches every execution inside it and a check finds the target
// among the ones that execution is inside. Where the `disable` lands past the
// body, the extent is a region binding the effect leaving the body: it lands
// the effect here when the `disable` it carries lands here, so execution
// continues past the region, and raises it again for the region where it does
// land. Where
// it lands at the execution's end, nothing binds it, and it ends the execution
// as an effect no region claims does. `target` is the cell the scope owns; a
// named block, a named fork, and a task each build one alike.
auto BuildTargetExtent(
    ProcessLowerer& process, const WalkFrame& frame, mir::Block&& body,
    const StaticStorageHome& target, support::TargetLanding landing)
    -> mir::StmtData;

// LRM 6.21: an automatic variable a detached fork branch borrows can outlive
// the scope that declares it, so each such variable among `declared` is
// promoted into a cell of its own, held by a shared pointer made in the frame's
// block, and the pointer is recorded so the variable's declaration and its
// references reach the cell through it. A branch keeps each cell it names alive
// by holding a by-value copy of its pointer. `declared` is every variable the
// scope being entered declares; which of them are promoted is the variables'
// own statement.
void OpenActivationScope(
    ProcessLowerer& process, const WalkFrame& frame,
    std::span<const hir::ProceduralVarId> declared);

auto LowerBlockStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::BlockStmt& b, support::TargetLanding landing)
    -> diag::Result<mir::Stmt>;

// Lowers `disable <named block or task>` (LRM 9.6.2) to the one call that
// carries the whole statement: it invalidates the named target, wakes or ends
// what is inside it, and leaves the disabling execution when that execution is
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
