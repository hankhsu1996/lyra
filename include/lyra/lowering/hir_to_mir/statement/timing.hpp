#pragma once

// Lowering of timing-control statements: timed statement `@(...) body` /
// `#N body` / `@e body` / `@* body` (LRM 9.4), `-> e;` event trigger
// (LRM 15.5.1), and `wait (cond) body` (LRM 9.4.3).

#include <optional>
#include <string>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

// The wait an event control is, built into `block` (LRM 9.4.2, 15.5.2). What a
// statement's control and an assignment's intra-assignment control share is the
// wait itself; what differs is only what follows it, so both reach these.
auto BuildEventWaitStmt(
    ProcessLowerer& process, WalkFrame frame, mir::Block& block,
    const hir::EventControl& ec) -> diag::Result<mir::Stmt>;

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
