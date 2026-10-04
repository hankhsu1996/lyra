#pragma once

// Lowering of loop statements (LRM 12.7): `for`, `while`, `do ... while`,
// `repeat`, and `forever`, and the counting loops other lowerings are built
// from.

#include <cstdint>
#include <optional>
#include <string>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/access_path.hpp"
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
// returned loop. `break_label` is the label a `break` that names this loop
// carries, absent where none does.
auto BuildCountingLoopStmt(
    const mir::CompilationUnit& unit, WalkFrame frame, mir::Block& block,
    mir::ExprId count, mir::LocalId position, mir::BlockId body_scope,
    std::optional<mir::LoopLabelId> break_label = std::nullopt) -> mir::Stmt;

// The same loop over a position that stands in storage of its own, which
// outlives the loop where something still reads it afterwards: `position` is
// the path to that storage, named in `block`, and evaluates nothing.
auto BuildCountingLoopStmt(
    mir::CompilationUnit& unit, WalkFrame frame, mir::Block& block,
    mir::ExprId count, const AccessPath& position, mir::BlockId body_scope,
    std::optional<mir::LoopLabelId> break_label) -> mir::Stmt;

// A loop over a declared range (LRM 7.4): the variable `index` reaches takes
// each index in turn, from `left` through `right` in the direction the range
// runs, and `body_scope` runs at each. `index` is a path as the stored position
// of a counting loop is, and `break_label` is as that loop's is.
auto BuildRangeLoopStmt(
    mir::CompilationUnit& unit, mir::Block& block, const AccessPath& index,
    std::int64_t left, std::int64_t right, mir::BlockId body_scope,
    std::optional<mir::LoopLabelId> break_label) -> mir::Stmt;

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
