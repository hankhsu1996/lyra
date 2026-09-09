#pragma once

// Lowering of timing-control statements: timed statement `@(...) body` /
// `#N body` / `@e body` / `@* body` (LRM 9.4), the `-> e;` and `->> e;` event
// triggers (LRM 15.5.1), and `wait (cond) body` (LRM 9.4.3).

#include <optional>
#include <string>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/expression/expr_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

// The wait an event control is, built into `block` (LRM 9.4.2, 15.5.2). What a
// statement's control and a deferred effect's `delay_or_event_control` share is
// the wait itself; what differs is only what follows it, so both reach these.
//
// A scope's own lowering builds one too: the process that samples a clocking
// event is synthesized there rather than written by anyone (LRM 16.9.3), and
// its clock is the same trigger set an `@(...)` is. So this is templated over
// the pass class for the reason every context-free handler here is -- the two
// build different things and the wait is not one of them.
//
// `scope` is the lowering of the scope whose storage the leaves live in: a
// procedural body reaches it through its enclosing scope, and a scope's own
// lowering already is it.
template <ExprLowerer Lowerer>
auto BuildEventWaitStmt(
    Lowerer& lowerer, const StructuralScopeLowerer& scope, WalkFrame frame,
    mir::Block& block, const hir::EventControl& ec) -> diag::Result<mir::Stmt>;

auto BuildNamedEventWaitStmt(
    ProcessLowerer& process, WalkFrame frame, mir::Block& block,
    const hir::NamedEventControl& nec) -> diag::Result<mir::Stmt>;

// The same wait for a caller holding an event control whose form it has no
// reason to know -- a repeat counts occurrences of either alike (LRM 9.4.5).
auto BuildAnyEventWaitStmt(
    ProcessLowerer& process, WalkFrame frame, mir::Block& block,
    const hir::AnyEventControl& event) -> diag::Result<mir::Stmt>;

auto LowerTimedStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::TimedStmt& t) -> diag::Result<mir::Stmt>;

auto LowerEventTriggerStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::EventTriggerStmt& et) -> diag::Result<mir::Stmt>;

auto LowerWaitStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::WaitStmt& w) -> diag::Result<mir::Stmt>;

}  // namespace lyra::lowering::hir_to_mir
