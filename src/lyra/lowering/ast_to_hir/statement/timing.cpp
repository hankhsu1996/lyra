#include "lyra/lowering/ast_to_hir/statement/timing.hpp"

#include <expected>
#include <optional>
#include <utility>
#include <variant>
#include <vector>

#include <slang/ast/Statement.h>
#include <slang/ast/TimingControl.h>
#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/statements/MiscStatements.h>
#include <slang/ast/types/Type.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/expr_builders.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/lowering/ast_to_hir/inferred_clocking.hpp"
#include "lyra/lowering/ast_to_hir/reads.hpp"
#include "lyra/lowering/ast_to_hir/structural_scope_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

auto LowerEventEdge(slang::ast::EdgeKind kind) -> support::EventEdge {
  switch (kind) {
    case slang::ast::EdgeKind::None:
      return support::EventEdge::kAnyChange;
    case slang::ast::EdgeKind::PosEdge:
      return support::EventEdge::kPosedge;
    case slang::ast::EdgeKind::NegEdge:
      return support::EventEdge::kNegedge;
    case slang::ast::EdgeKind::BothEdges:
      return support::EventEdge::kBothEdges;
  }
  throw InternalError("LowerEventEdge: unknown slang EdgeKind value");
}

// LRM 9.4.2.3: the `iff` qualifier, which an occurrence must hold for to be an
// event. What it reads is no part of the wait's sensitivity -- the standard
// evaluates it where the occurrence happens and not when the qualifier itself
// moves, so a change in the qualifier alone reaches nothing.
template <typename Lowerer>
auto LowerEventCondition(
    Lowerer& lowerer, WalkFrame frame,
    const slang::ast::SignalEventControl& sig)
    -> diag::Result<std::optional<hir::Expr>> {
  if (sig.iffCondition == nullptr) {
    return std::nullopt;
  }
  auto cond_or = lowerer.LowerExpr(*sig.iffCondition, frame);
  if (!cond_or) return std::unexpected(std::move(cond_or.error()));
  return std::optional<hir::Expr>{*std::move(cond_or)};
}

// What a value entry waits for: a change in the value of its operand.
template <typename Lowerer>
auto LowerValueChange(
    Lowerer& lowerer, WalkFrame frame,
    const slang::ast::SignalEventControl& sig, diag::SourceSpan span)
    -> diag::Result<hir::ValueChange> {
  auto expr_or = lowerer.LowerExpr(sig.expr, frame);
  if (!expr_or) return std::unexpected(std::move(expr_or.error()));

  // An edge is a transition of the expression's least significant bit, so it
  // needs an integral operand -- which binding already requires, so an edge
  // carries no restriction of its own. What remains is the control without one,
  // which reacts to the whole value (LRM 9.4.2) and so needs one whose change
  // is answered. What is left out is an operand that is no value at all and one
  // whose value nothing yet compares.
  const auto& expr_type = lowerer.Owner().Unit().types.Get(expr_or->type);
  if (sig.edge == slang::ast::EdgeKind::None &&
      !expr_type.IsValueChangeObservable()) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedEventTriggerForm,
        "an event control on a value of this type is not yet supported");
  }

  // What the expression reads is what makes the wait a candidate; the edge
  // belongs to the expression, whose value decides.
  auto cells = CellsOfWaitedExpression(lowerer, frame, sig.expr);
  if (!cells) return std::unexpected(std::move(cells.error()));

  return hir::ValueChange{
      .signal = frame.Exprs().Add(*std::move(expr_or)),
      .edge = LowerEventEdge(sig.edge),
      .cells = *std::move(cells),
  };
}

// What an event entry waits for: a trigger of the object its operand names
// where the control is reached, rather than a change in what the operand
// reads, which is no occurrence (LRM 15.5.1). An edge needs an integral
// operand, which an event is not, so binding refuses one and no polarity
// reaches this far.
template <typename Lowerer>
auto LowerNamedEventTrigger(
    Lowerer& lowerer, WalkFrame frame,
    const slang::ast::SignalEventControl& sig)
    -> diag::Result<hir::NamedEventTrigger> {
  auto expr_or = lowerer.LowerExpr(sig.expr, frame);
  if (!expr_or) return std::unexpected(std::move(expr_or.error()));
  return hir::NamedEventTrigger{
      .event = frame.Exprs().Add(*std::move(expr_or))};
}

// The entry `sig` is: what it waits for, decided by the type of its operand
// (LRM 9.4.2, 15.5.2), which slang gives the same shape, and its qualifier.
template <typename Lowerer>
auto LowerEventEntry(
    Lowerer& lowerer, WalkFrame frame,
    const slang::ast::SignalEventControl& sig, diag::SourceSpan span)
    -> diag::Result<hir::EventEntry> {
  using Occurrence = std::variant<hir::ValueChange, hir::NamedEventTrigger>;
  auto occurrence = [&]() -> diag::Result<Occurrence> {
    if (sig.expr.type->isEvent()) {
      return LowerNamedEventTrigger(lowerer, frame, sig);
    }
    return LowerValueChange(lowerer, frame, sig, span);
  }();
  if (!occurrence) return std::unexpected(std::move(occurrence.error()));
  auto condition = LowerEventCondition(lowerer, frame, sig);
  if (!condition) return std::unexpected(std::move(condition.error()));
  return hir::EventEntry{
      .occurrence = *std::move(occurrence),
      .condition = condition->has_value()
                       ? std::optional<hir::ExprId>{frame.Exprs().Add(
                             **std::move(condition))}
                       : std::nullopt};
}

// LRM 9.4.1 `#N`. The amount is an ordinary expression, read where the control
// is, so nothing about the scope's time unit is decided here.
auto LowerDelayControl(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::DelayControl& delay) -> diag::Result<hir::DelayControl> {
  auto duration = proc.LowerExpr(delay.expr, frame);
  if (!duration) return std::unexpected(std::move(duration.error()));
  return hir::DelayControl{.duration = frame.Exprs().Add(*std::move(duration))};
}

// `controlled` is the statement the control gates. Only `@*` reads it: LRM
// 9.4.2.2 defines its sensitivity as the reads of that statement, so the
// control cannot be built until the statement has lowered and each read's
// reference is resolved.
auto LowerTimingControl(
    ProcessLowerer& proc, WalkFrame frame, const slang::ast::TimingControl& tc,
    const slang::ast::Statement& controlled, diag::SourceSpan span)
    -> diag::Result<hir::TimingControl> {
  switch (tc.kind) {
    case slang::ast::TimingControlKind::Delay: {
      auto delay_or =
          LowerDelayControl(proc, frame, tc.as<slang::ast::DelayControl>());
      if (!delay_or) return std::unexpected(std::move(delay_or.error()));
      return hir::TimingControl{*delay_or};
    }
    case slang::ast::TimingControlKind::SignalEvent:
    case slang::ast::TimingControlKind::EventList: {
      auto event_or = LowerEventControl(proc, frame, tc, span);
      if (!event_or) return std::unexpected(std::move(event_or.error()));
      return hir::TimingControl{*std::move(event_or)};
    }
    case slang::ast::TimingControlKind::ImplicitEvent: {
      const auto& reads = proc.Owner().Sensitivity().AnalyzeImplicitEventList(
          controlled, proc.ContainingSymbol());
      auto sensitivity = proc.Owner().SensitivityEntriesOf(proc, reads, frame);
      if (!sensitivity) return std::unexpected(std::move(sensitivity.error()));
      return hir::TimingControl{hir::ImplicitEventControl{
          .sensitivity_list = *std::move(sensitivity)}};
    }
    case slang::ast::TimingControlKind::RepeatedEvent:
      // LRM A.6.5: a repeat count stands only inside a whole
      // `delay_or_event_control` -- what prefixes a statement is a delay, an
      // event control, or a cycle delay -- so the form that reads one is below.
      throw InternalError(
          "LowerTimingControl: a repeated event control reached statement "
          "timing, where the grammar does not put one");
    default:
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedTimingControlKind,
          "this kind of timing control is not yet supported");
  }
}

// LRM 15.5.1: when the trigger happens. The grammar puts a control only on the
// nonblocking form, which is the one whose effect is due in a slot other than
// where the statement stands, so a blocking form carrying one is a shape the
// source cannot have written.
auto LowerTriggerTiming(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::EventTriggerStatement& et, diag::SourceSpan span)
    -> diag::Result<hir::EffectTiming> {
  if (!et.isNonBlocking) {
    if (et.timing != nullptr) {
      throw InternalError(
          "LowerTriggerTiming: a blocking event trigger carrying a timing "
          "control reached lowering, where the grammar admits none");
    }
    return hir::EffectTiming{hir::ImmediateEffect{}};
  }
  if (et.timing == nullptr) {
    return hir::EffectTiming{hir::NonBlockingEffect{}};
  }
  auto control = LowerDelayOrEventControl(proc, frame, *et.timing, span);
  if (!control) return std::unexpected(std::move(control.error()));
  return hir::EffectTiming{
      hir::NonBlockingEffect{.control = *std::move(control)}};
}

}  // namespace

// Every position the grammar admits an event control -- in front of a
// statement, inside an assignment, after a nonblocking trigger's operator,
// behind a repeat count, and as the event a sampled value function counts ticks
// of -- reaches it here, so what one is written to mean is settled once.
//
// LRM 9.4.2.1 `@(a or b)` / `@(a, b)`: every entry watches for its own event
// and the wait ends on the first of them. A clocking event names its entries
// through what `$inferred_clock` stands for, so they are taken resolved.
template <typename Lowerer>
auto LowerEventControl(
    Lowerer& lowerer, WalkFrame frame, const slang::ast::TimingControl& tc,
    diag::SourceSpan span) -> diag::Result<hir::EventControl> {
  auto events = ResolvedClockEntries(frame, tc, span);
  if (!events) return std::unexpected(std::move(events.error()));
  hir::EventControl control;
  for (const slang::ast::SignalEventControl* event : *events) {
    auto entry = LowerEventEntry(lowerer, frame, *event, span);
    if (!entry) return std::unexpected(std::move(entry.error()));
    control.entries.push_back(*std::move(entry));
  }
  return control;
}

template auto LowerEventControl(
    ProcessLowerer&, WalkFrame, const slang::ast::TimingControl&,
    diag::SourceSpan) -> diag::Result<hir::EventControl>;
template auto LowerEventControl(
    StructuralScopeLowerer&, WalkFrame, const slang::ast::TimingControl&,
    diag::SourceSpan) -> diag::Result<hir::EventControl>;

auto LowerDelayOrEventControl(
    ProcessLowerer& proc, WalkFrame frame, const slang::ast::TimingControl& tc,
    diag::SourceSpan span) -> diag::Result<hir::DelayOrEventControl> {
  switch (tc.kind) {
    case slang::ast::TimingControlKind::Delay: {
      auto delay_or =
          LowerDelayControl(proc, frame, tc.as<slang::ast::DelayControl>());
      if (!delay_or) return std::unexpected(std::move(delay_or.error()));
      return hir::DelayOrEventControl{*delay_or};
    }
    case slang::ast::TimingControlKind::SignalEvent:
    case slang::ast::TimingControlKind::EventList: {
      auto event_or = LowerEventControl(proc, frame, tc, span);
      if (!event_or) return std::unexpected(std::move(event_or.error()));
      return hir::DelayOrEventControl{*std::move(event_or)};
    }
    case slang::ast::TimingControlKind::RepeatedEvent: {
      const auto& repeated = tc.as<slang::ast::RepeatedEventControl>();
      // LRM 9.4.5 reads the count once, where the statement is reached, so
      // changing what it read afterwards does not move the number of
      // occurrences still to come.
      auto count_or = proc.LowerExpr(repeated.expr, frame);
      if (!count_or) return std::unexpected(std::move(count_or.error()));
      const hir::ExprId count = frame.Exprs().Add(*std::move(count_or));
      auto event_or = LowerEventControl(proc, frame, repeated.event, span);
      if (!event_or) return std::unexpected(std::move(event_or.error()));
      return hir::DelayOrEventControl{hir::RepeatedEventControl{
          .count = count, .event = *std::move(event_or)}};
    }
    default:
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedTimingControlKind,
          "this kind of timing control is not yet supported where a delay or "
          "an event control may stand");
  }
}

auto LowerIntraAssignmentStmt(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::AssignmentExpression& as, diag::SourceSpan span)
    -> diag::Result<hir::Stmt> {
  auto type_or = proc.Owner().InternExpressionType(as, span);
  if (!type_or) return std::unexpected(std::move(type_or.error()));
  const hir::TypeId type = *type_or;

  auto& body = *frame.current_procedural_body;
  OpenProceduralScope scope{
      frame.ProceduralScopes().Declare(), hir::ProceduralScopeKind::kBlock,
      std::nullopt};
  const WalkFrame inner = frame.WithOpenScope(&scope);

  const hir::ProceduralVarId held = body.procedural_vars.Declare();
  body.procedural_vars.Define(
      held, hir::ProceduralVarDecl{
                .name = std::nullopt,
                .type = type,
                .lifetime = hir::VariableLifetime::kAutomatic});
  inner.OpenScope().declarations.push_back(held);

  const auto held_ref = [&] {
    return inner.Exprs().Add(
        hir::MakeRefExpr(hir::ProceduralVarRef{.var = held}, type, span));
  };
  const auto store = [&](hir::ExprId lhs, hir::ExprId rhs) -> hir::StmtId {
    const hir::ExprId assign = inner.Exprs().Add(
        hir::Expr{
            .type = type,
            .data =
                hir::AssignExpr{
                    .timing = hir::ImmediateEffect{},
                    .lhs = lhs,
                    .compound = std::nullopt,
                    .rhs = rhs},
            .span = span});
    return body.stmts.Add(
        hir::Stmt{
            .label = std::nullopt,
            .data = hir::ExprStmt{.expr = assign},
            .span = span});
  };
  const auto plain = [&](hir::StmtData data) -> hir::StmtId {
    return body.stmts.Add(
        hir::Stmt{
            .label = std::nullopt, .data = std::move(data), .span = span});
  };

  std::vector<hir::StmtId> statements;
  statements.push_back(plain(hir::VarDeclStmt{.var = held}));

  auto rhs_or = proc.LowerExpr(as.right(), inner);
  if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
  statements.push_back(
      store(held_ref(), inner.Exprs().Add(*std::move(rhs_or))));

  // The assignment itself runs under the control, which is what LRM 10.4.1 asks
  // for: a left side that needs evaluating -- an index, a class handle, a
  // virtual interface reference -- is evaluated where the control is satisfied,
  // not where the statement is reached.
  auto lhs_or = proc.LowerExpr(as.left(), inner);
  if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
  const hir::StmtId assign =
      store(inner.Exprs().Add(*std::move(lhs_or)), held_ref());

  auto control = LowerDelayOrEventControl(proc, inner, *as.timingControl, span);
  if (!control) return std::unexpected(std::move(control.error()));

  // Each form of the control says how the assignment reaches its own
  // suspension. Three of them are a single wait, which prefixes the assignment
  // as one statement; the repeat form is a count of waits, which no single
  // timing control spells, so it becomes a loop over an empty body with the
  // assignment after it -- and LRM 9.4.5's count of none then reaches the
  // assignment where the statement stands, by the loop not iterating.
  const auto wait_then_assign = [&](hir::TimingControl timing) {
    statements.push_back(
        plain(hir::TimedStmt{.timing = std::move(timing), .stmt = assign}));
  };
  std::visit(
      Overloaded{
          [&](hir::DelayControl d) { wait_then_assign(d); },
          [&](hir::EventControl e) { wait_then_assign(std::move(e)); },
          [&](hir::RepeatedEventControl r) {
            const hir::StmtId nothing = plain(hir::EmptyStmt{});
            const hir::StmtId wait = plain(
                hir::TimedStmt{
                    .timing = hir::TimingControl{std::move(r.event)},
                    .stmt = nothing});
            statements.push_back(
                plain(hir::RepeatStmt{.count = r.count, .body = wait}));
            statements.push_back(assign);
          }},
      *std::move(control));

  return hir::Stmt{
      .label = std::nullopt,
      .data =
          hir::BlockStmt{
              .statements = std::move(statements),
              .scope = frame.SealScope(std::move(scope))},
      .span = span};
}

auto LowerTimedStmt(
    ProcessLowerer& proc, WalkFrame frame, const slang::ast::TimedStatement& ts,
    diag::SourceSpan span) -> diag::Result<hir::Stmt> {
  // The controlled statement lowers first: a control whose sensitivity is
  // inferred from it needs each read's reference already resolved.
  auto inner_stmt = proc.LowerStmt(ts.stmt, frame);
  if (!inner_stmt) return std::unexpected(std::move(inner_stmt.error()));
  const hir::StmtId inner_id =
      frame.current_procedural_body->stmts.Add(*std::move(inner_stmt));
  auto timing = LowerTimingControl(proc, frame, ts.timing, ts.stmt, span);
  if (!timing) return std::unexpected(std::move(timing.error()));
  return hir::Stmt{
      .label = std::nullopt,
      .data = hir::TimedStmt{.timing = *std::move(timing), .stmt = inner_id},
      .span = span};
}

// LRM 15.5.1 `-> e;` and `->> [ delay_or_event_control ] e;`.
auto LowerEventTriggerStmt(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::EventTriggerStatement& et, diag::SourceSpan span)
    -> diag::Result<hir::Stmt> {
  auto expr_or = proc.LowerExpr(et.target, frame);
  if (!expr_or) return std::unexpected(std::move(expr_or.error()));
  auto timing_or = LowerTriggerTiming(proc, frame, et, span);
  if (!timing_or) return std::unexpected(std::move(timing_or.error()));
  return hir::Stmt{
      .label = std::nullopt,
      .data =
          hir::EventTriggerStmt{
              .event = frame.Exprs().Add(*std::move(expr_or)),
              .timing = *std::move(timing_or),
          },
      .span = span};
}

// LRM 9.4.3 `wait (cond) body`. The wait re-evaluates when anything the
// condition reads changes, so what it watches is that condition's own reads --
// narrower than the enclosing body's, which is why they are stated here rather
// than inherited.
auto LowerWaitStmt(
    ProcessLowerer& proc, WalkFrame frame, const slang::ast::WaitStatement& w,
    diag::SourceSpan span) -> diag::Result<hir::Stmt> {
  auto cond_or = proc.LowerExpr(w.cond, frame);
  if (!cond_or) return std::unexpected(std::move(cond_or.error()));
  const hir::ExprId cond_id = frame.Exprs().Add(*std::move(cond_or));
  auto body_or = proc.LowerStmt(w.stmt, frame);
  if (!body_or) return std::unexpected(std::move(body_or.error()));
  const hir::StmtId body_id =
      frame.current_procedural_body->stmts.Add(*std::move(body_or));
  auto cells = CellsOfWaitedExpression(proc, frame, w.cond);
  if (!cells) return std::unexpected(std::move(cells.error()));
  return hir::Stmt{
      .label = std::nullopt,
      .data =
          hir::WaitStmt{
              .cond = cond_id, .body = body_id, .cells = *std::move(cells)},
      .span = span};
}

}  // namespace lyra::lowering::ast_to_hir
