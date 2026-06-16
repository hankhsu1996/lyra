#pragma once

// Lowering of timing-control statements: timed statement `@(...) body` /
// `#N body` / `@e body` / `@* body` (LRM 9.4), `-> e;` event trigger
// (LRM 15.5.1), and `wait (cond) body` (LRM 9.4.3).

#include <optional>
#include <string>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerTimedStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    diag::SourceSpan span, const hir::TimedStmt& t) -> diag::Result<mir::Stmt>;

auto LowerEventTriggerStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::EventTriggerStmt& et) -> diag::Result<mir::Stmt>;

auto LowerWaitStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::WaitStmt& w) -> diag::Result<mir::Stmt>;

}  // namespace lyra::lowering::hir_to_mir
