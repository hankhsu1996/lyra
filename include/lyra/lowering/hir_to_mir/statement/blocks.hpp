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
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerEmptyStmt(std::optional<std::string> label)
    -> diag::Result<mir::Stmt>;

// The lvalue reaching a scope's cancellation source, the expression a region
// names to recognize the control effect it consumes and a `disable` names to
// invalidate (LRM 9.6.2).
auto CancellationSourceAccess(
    const ProcessLowerer& process, const WalkFrame& frame,
    StaticStoragePlacement placement) -> mir::ExprId;

// Declares a target's entry guard (LRM 9.6.2): a local that marks the executing
// process as inside the target for the guard's lifetime, so a `disable` of the
// target reaches this execution and a check finds the target among the ones it
// is inside. A named block and a task install one alike, as the first act of
// the body the target names.
auto EmitCancellationGuard(
    ProcessLowerer& process, const WalkFrame& frame,
    StaticStoragePlacement placement) -> mir::LocalId;

// Builds the region that consumes a `disable` of the target the placement names
// (LRM 9.6.2) around an already-lowered `body`: it binds the effect leaving the
// body and hands it to the consume, which either ends the effect here -- so
// execution continues past the region -- or re-raises it for the region that
// does name its target.
auto MakeCancellableRegion(
    ProcessLowerer& process, const WalkFrame& frame, mir::BlockId body,
    StaticStoragePlacement placement) -> mir::TryStmt;

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
