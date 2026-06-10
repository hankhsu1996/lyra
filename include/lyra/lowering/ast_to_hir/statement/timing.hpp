#pragma once

// Lowering of timing-control-bearing statements:
//   - TimedStatement (LRM 9.4)
//   - SignalEventControl (`@(...)`) and NamedEventControl (`@e`)
//   - WaitStatement (LRM 9.4.3)
//   - EventTriggerStatement (LRM 15.5.1)

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace slang::ast {
class EventTriggerStatement;
class TimedStatement;
class WaitStatement;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

auto LowerTimedStmt(
    ProcessLowerer& proc, WalkFrame frame, const slang::ast::TimedStatement& ts,
    diag::SourceSpan span) -> diag::Result<hir::Stmt>;

auto LowerEventTriggerStmt(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::EventTriggerStatement& et, diag::SourceSpan span)
    -> diag::Result<hir::Stmt>;

auto LowerWaitStmt(
    ProcessLowerer& proc, WalkFrame frame, const slang::ast::WaitStatement& w,
    diag::SourceSpan span) -> diag::Result<hir::Stmt>;

}  // namespace lyra::lowering::ast_to_hir
