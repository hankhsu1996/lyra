#pragma once

// Lowering of loop statements (LRM 12.7): `for`, `while`, `do ... while`,
// `repeat`, and `forever`. `foreach` lowers upstream (AST -> HIR) into a
// `for` shape and never reaches this family.

#include <optional>
#include <string>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/block_id.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

// A loop that counts out the positions below `count` and runs `body_scope` at
// each, with the count read once before the first iteration. `position` is the
// caller's local, so a body that names which iteration it is reads that local
// and one that does not simply leaves it alone. It takes an already lowered
// count and an already built body, so a caller whose body is not a source
// statement reaches the same loop as one whose body is. The declarations it
// needs are appended to `block`, whose statements must therefore run before the
// returned loop.
auto BuildCountingLoopStmt(
    const mir::CompilationUnit& unit, WalkFrame frame, mir::Block& block,
    mir::ExprId count, mir::LocalId position, mir::BlockId body_scope)
    -> mir::Stmt;

// The same loop for a body that runs a number of times and never asks which
// time this is (LRM 12.7.3, and the repeat event control of LRM 9.4.5).
auto BuildRepeatLoopStmt(
    const mir::CompilationUnit& unit, WalkFrame frame, mir::Block& block,
    mir::ExprId count, mir::BlockId body_scope) -> mir::Stmt;

auto LowerForStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::ForStmt& f) -> diag::Result<mir::Stmt>;

auto LowerWhileStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::WhileStmt& w) -> diag::Result<mir::Stmt>;

auto LowerDoWhileStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::DoWhileStmt& d) -> diag::Result<mir::Stmt>;

auto LowerRepeatStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::RepeatStmt& r) -> diag::Result<mir::Stmt>;

auto LowerForeverStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::ForeverStmt& f) -> diag::Result<mir::Stmt>;

}  // namespace lyra::lowering::hir_to_mir
