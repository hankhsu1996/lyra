#pragma once

// Lowering of the timing controls themselves (LRM 9.4), wherever the grammar
// puts one, and of the statements that carry one:
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
class AssignmentExpression;
class EventTriggerStatement;
class TimedStatement;
class TimingControl;
class WaitStatement;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// LRM 9.4.5 `delay_or_event_control`: the control an assignment carries between
// its operator and its right-hand side. Both assignment forms read it here --
// the blocking one to suspend the procedure at it, the nonblocking one to say
// which slot's NBA region the update lands in.
auto LowerIntraAssignmentControl(
    ProcessLowerer& proc, WalkFrame frame, const slang::ast::TimingControl& tc,
    diag::SourceSpan span) -> diag::Result<hir::IntraAssignmentControl>;

// LRM 9.4.5 Table 9-3: a blocking assignment carrying an intra-assignment
// timing control is the same program as holding the right-hand side in a
// temporary, applying the control, and then assigning.
auto LowerIntraAssignmentStmt(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::AssignmentExpression& as, diag::SourceSpan span)
    -> diag::Result<hir::Stmt>;

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
