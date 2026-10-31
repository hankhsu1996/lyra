#pragma once

// Lowering of timing-control statements: timed statement `@(...) body` /
// `#N body` / `@e body` / `@* body` (LRM 9.4), the `-> e;` and `->> e;` event
// triggers (LRM 15.5.1), and `wait (cond) body` (LRM 9.4.3).

#include <optional>
#include <string>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/expression/expr_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/sensitivity_wait.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/builder.hpp"
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
template <ExprLowerer Lowerer>
auto BuildEventWaitStmt(
    Lowerer& lowerer, WalkFrame frame, mir::Builder block,
    const hir::EventControl& ec) -> diag::Result<mir::Stmt>;

// Whether a change to what an event control watches can decide on its own that
// it was an event, which it can where everything the control evaluates only
// reads storage; otherwise the waiting process decides (LRM 4.5, 4.7).
[[nodiscard]] auto ChangeDecides(
    const base::Arena<hir::Expr, hir::ExprId>& exprs,
    const hir::EventControl& ec) -> bool;

// The leaves an event control a change decides watches, each paired with the
// observation of the entry it belongs to, declared in the block `frame` is
// writing: what a wait built from them enrols on. An entry naming an event
// evaluates the event there, so a wait built from leaves of one is built where
// the control is reached.
template <ExprLowerer Lowerer>
auto BuildEventLeaves(
    Lowerer& lowerer, const WalkFrame& frame, const hir::EventControl& ec)
    -> diag::Result<std::vector<ObservedLeaf>>;

// The wait for the next trigger of the event object `event`, a value in
// `block` evaluated where the wait is reached, built into `block` (LRM 15.5.2):
// the wait an event control naming that one event is, for a waiter the
// lowering writes rather than the source.
template <ExprLowerer Lowerer>
auto BuildEventObjectWaitStmt(
    Lowerer& lowerer, WalkFrame frame, mir::Builder block, mir::ExprId event)
    -> diag::Result<mir::Stmt>;

// LRM 15.5.1: triggering the object the event value `event_id`, in `block`,
// names, which wakes what waits on it.
auto BuildTriggerCallExpr(
    UnitLowerer& unit_lowerer, mir::Builder block, mir::ExprId event_id)
    -> mir::Expr;

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
