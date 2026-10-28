#pragma once

// Lowering of the timing controls themselves (LRM 9.4), wherever the grammar
// puts one, and of the statements that carry one:
//   - TimedStatement (LRM 9.4)
//   - SignalEventControl and EventListControl (`@e`, `@(...)`)
//   - WaitStatement (LRM 9.4.3)
//   - EventTriggerStatement (LRM 15.5.1)

#include <slang/ast/SemanticFacts.h>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"
#include "lyra/support/event_edge.hpp"

namespace slang::ast {
class AssignmentExpression;
class EventTriggerStatement;
class TimedStatement;
class TimingControl;
class WaitStatement;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// Which transition of an event's operand the event is (LRM 9.4.2).
auto LowerEventEdge(slang::ast::EdgeKind kind) -> support::EventEdge;

// LRM A.6.5 `delay_or_event_control`: the control a statement carries towards
// deciding when its effect happens. Every position the grammar admits one reads
// it here -- an assignment between its operator and its right-hand side, a
// nonblocking event trigger after its operator -- so that a blocking form
// suspends the procedure at it and a nonblocking one says which slot's NBA
// region the effect lands in.
auto LowerDelayOrEventControl(
    ProcessLowerer& proc, WalkFrame frame, const slang::ast::TimingControl& tc,
    diag::SourceSpan span) -> diag::Result<hir::DelayOrEventControl>;

// An `event_control` (LRM 9.4.2), each of whose entries waits for a change in a
// value or for an event to be triggered (LRM 15.5.2). A clocking event is one
// of these by whichever of LRM 16.9.3's rules supplied it -- a timing control
// the front end built, or the clocking event another instance's clocking block
// publishes -- so counting its ticks needs no second vocabulary. One a sampled
// value function counts ticks of may stand outside any procedure, so it is
// lowered wherever an expression is.
template <typename Lowerer>
auto LowerEventControl(
    Lowerer& lowerer, WalkFrame frame, const ClockingEvent& clock,
    diag::SourceSpan span) -> diag::Result<hir::EventControl>;

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
