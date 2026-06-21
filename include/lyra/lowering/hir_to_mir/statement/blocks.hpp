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
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerEmptyStmt(std::optional<std::string> label)
    -> diag::Result<mir::Stmt>;

auto LowerBlockStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::BlockStmt& b) -> diag::Result<mir::Stmt>;

// Lowers a single HIR stmt into its own fresh `mir::Block`: opens
// a child block, descends through
// `frame.WithBlock(...).Deeper()` so local refs see the correct
// hop count, and packages the result as the new block's sole root stmt.
// Used wherever a control-flow node needs a body or branch scope (for,
// while, repeat, do-while, forever, if branches, case items).
auto LowerStmtIntoChildScope(
    ProcessLowerer& process, WalkFrame frame, hir::StmtId hir_stmt_id)
    -> diag::Result<mir::Block>;

}  // namespace lyra::lowering::hir_to_mir
