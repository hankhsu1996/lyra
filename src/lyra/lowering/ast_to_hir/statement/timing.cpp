#include "lyra/lowering/ast_to_hir/statement/timing.hpp"

#include <expected>
#include <optional>
#include <string>
#include <string_view>
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
#include "lyra/lowering/ast_to_hir/expression/assignment.hpp"
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

// LRM 9.4.2.3: the `iff` qualifier, which the change must hold for to be an
// event. What it reads is no part of the wait's sensitivity -- the standard
// evaluates it where the watched expression moves and not when the qualifier
// itself does, so a change in the qualifier alone reaches nothing.
auto LowerEventCondition(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::SignalEventControl& sig)
    -> diag::Result<std::optional<hir::Expr>> {
  if (sig.iffCondition == nullptr) {
    return std::nullopt;
  }
  auto cond_or = proc.LowerExpr(*sig.iffCondition, frame);
  if (!cond_or) return std::unexpected(std::move(cond_or.error()));
  return std::optional<hir::Expr>{*std::move(cond_or)};
}

auto AddEventCondition(WalkFrame frame, std::optional<hir::Expr> condition)
    -> std::optional<hir::ExprId> {
  if (!condition.has_value()) {
    return std::nullopt;
  }
  return frame.Exprs().Add(*std::move(condition));
}

auto LowerSignalEventTrigger(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::SignalEventControl& sig, diag::SourceSpan span)
    -> diag::Result<hir::EventTrigger> {
  auto expr_or = proc.LowerExpr(sig.expr, frame);
  if (!expr_or) return std::unexpected(std::move(expr_or.error()));

  const auto& expr_type = proc.Owner().Unit().types.Get(expr_or->type);
  if (sig.edge != slang::ast::EdgeKind::None) {
    // The runtime classifies an edge only on a packed bit-vector cell (LRM
    // 9.4.2 Table 9-2); slang already restricts an edge to an integral operand.
    if (!expr_type.IsBitVector()) {
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedEventTriggerForm,
          "edge event control is only supported on a packed bit-vector "
          "operand");
    }
  } else if (!expr_type.IsValueChangeObservable()) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedEventTriggerForm,
        "value-change event control on a non-value operand is not yet "
        "supported");
  }

  const auto edge_kind = LowerEventEdge(sig.edge);

  // The leaves are what the expression reads, which is what makes the wait a
  // candidate; the edge belongs to the expression, whose value decides.
  const auto& reads = proc.Owner().Sensitivity().AnalyzeReads(
      sig.expr, proc.ContainingSymbol());
  auto sensitivity_list = proc.Owner().TranslateSensitivityReads(reads, frame);
  if (!sensitivity_list) {
    return std::unexpected(std::move(sensitivity_list.error()));
  }

  auto condition = LowerEventCondition(proc, frame, sig);
  if (!condition) return std::unexpected(std::move(condition.error()));

  return hir::EventTrigger{
      .signal = frame.Exprs().Add(*std::move(expr_or)),
      .edge = edge_kind,
      .sensitivity_list = *std::move(sensitivity_list),
      .condition = AddEventCondition(frame, *std::move(condition)),
  };
}

// The storage a plain reference designates, for a construct that names a cell
// rather than reading a value out of one -- a wait registering on it, a trigger
// occurring at it. Nothing but a bare name reaches storage that way.
auto AsWatchedStorage(const hir::Expr& expr)
    -> std::optional<hir::ReferenceRoute> {
  const auto* primary = std::get_if<hir::PrimaryExpr>(&expr.data);
  if (primary == nullptr) {
    return std::nullopt;
  }
  if (const auto* direct = std::get_if<hir::DirectMemberRef>(&primary->data)) {
    return hir::ReferenceRoute{*direct};
  }
  if (const auto* routed = std::get_if<hir::RoutedRef>(&primary->data)) {
    return hir::ReferenceRoute{*routed};
  }
  return std::nullopt;
}

// LRM 15.5.2 `@e;` on a named event. Distinguished from value-change `@(sig)`
// by the controlled expression's type. A trigger is the event itself, so the
// control watches the event's storage and no value is read from it -- and no
// edge polarity applies, which is why an edge qualifier is rejected here.
auto LowerNamedEventControl(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::SignalEventControl& sig, diag::SourceSpan span)
    -> diag::Result<hir::NamedEventControl> {
  if (sig.edge != slang::ast::EdgeKind::None) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedEventTriggerForm,
        "edge specifier is not valid on a named event");
  }

  auto expr_or = proc.LowerExpr(sig.expr, frame);
  if (!expr_or) return std::unexpected(std::move(expr_or.error()));

  const std::optional<hir::ReferenceRoute> route = AsWatchedStorage(*expr_or);
  if (!route.has_value()) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedEventTriggerForm,
        "named event reference must be a plain structural variable");
  }
  auto condition = LowerEventCondition(proc, frame, sig);
  if (!condition) return std::unexpected(std::move(condition.error()));
  return hir::NamedEventControl{
      .event = hir::SensitivityEntry{.ref = *route, .footprint = std::nullopt},
      .condition = AddEventCondition(frame, *std::move(condition)),
  };
}

// One `@(...)` entry, which is a named event or a value change by the type of
// what it names (LRM 15.5.2, 9.4.2); slang gives both the same shape.
auto LowerEventEntry(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::SignalEventControl& sig, diag::SourceSpan span)
    -> diag::Result<hir::AnyEventControl> {
  if (sig.expr.type->isEvent()) {
    auto nec_or = LowerNamedEventControl(proc, frame, sig, span);
    if (!nec_or) return std::unexpected(std::move(nec_or.error()));
    return *std::move(nec_or);
  }
  auto trigger_or = LowerSignalEventTrigger(proc, frame, sig, span);
  if (!trigger_or) return std::unexpected(std::move(trigger_or.error()));
  return hir::EventControl{.triggers = {*std::move(trigger_or)}};
}

// LRM 9.4.2.1 `@(a or b)` / `@(a, b)`: every entry watches for its own event
// and the wait ends on the first of them.
auto LowerEventListControl(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::EventListControl& list, diag::SourceSpan span)
    -> diag::Result<hir::EventControl> {
  std::vector<hir::EventTrigger> triggers;
  triggers.reserve(list.events.size());
  for (const auto* event : list.events) {
    if (event->kind != slang::ast::TimingControlKind::SignalEvent) {
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedTimingControlKind,
          "event list entries must be signal events; nested timing "
          "controls are not yet supported");
    }
    auto trigger_or = LowerSignalEventTrigger(
        proc, frame, event->as<slang::ast::SignalEventControl>(), span);
    if (!trigger_or) return std::unexpected(std::move(trigger_or.error()));
    triggers.push_back(*std::move(trigger_or));
  }
  return hir::EventControl{.triggers = std::move(triggers)};
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
      return std::visit(
          [](auto event) { return hir::TimingControl{std::move(event)}; },
          *std::move(event_or));
    }
    case slang::ast::TimingControlKind::ImplicitEvent: {
      const auto& reads = proc.Owner().Sensitivity().AnalyzeReads(
          controlled, proc.ContainingSymbol());
      auto sensitivity = proc.Owner().TranslateSensitivityReads(reads, frame);
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
          "this timing control kind is not yet supported");
  }
}

// The statement-prefix spelling of a `delay_or_event_control`, for the blocking
// form, whose control is a suspension of the procedure and so a statement. The
// repeat form has no such spelling -- it is a count of suspensions rather than
// one -- and the caller expands it.
auto AsStatementTiming(const hir::DelayOrEventControl& control)
    -> hir::TimingControl {
  return std::visit(
      Overloaded{
          [](const hir::RepeatedEventControl&) -> hir::TimingControl {
            throw InternalError(
                "AsStatementTiming: a repeat event control is a count of "
                "waits, which no single timing control spells");
          },
          [](const auto& plain) -> hir::TimingControl { return plain; }},
      control);
}

// The name a held right-hand side carries. LRM 9.4.5 gives it no name of its
// own, so one that cannot collide with a design's is minted here.
constexpr std::string_view kHeldValueName = "_lyra_intra_assign";

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
auto LowerEventControl(
    ProcessLowerer& proc, WalkFrame frame, const slang::ast::TimingControl& tc,
    diag::SourceSpan span) -> diag::Result<hir::AnyEventControl> {
  if (tc.kind == slang::ast::TimingControlKind::EventList) {
    auto list_or = LowerEventListControl(
        proc, frame, tc.as<slang::ast::EventListControl>(), span);
    if (!list_or) return std::unexpected(std::move(list_or.error()));
    return *std::move(list_or);
  }
  return LowerEventEntry(
      proc, frame, tc.as<slang::ast::SignalEventControl>(), span);
}

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
      return std::visit(
          [](auto event) { return hir::DelayOrEventControl{std::move(event)}; },
          *std::move(event_or));
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
          "this timing control kind is not yet supported where a delay or "
          "event control may stand");
  }
}

auto LowerIntraAssignmentStmt(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::AssignmentExpression& as, diag::SourceSpan span)
    -> diag::Result<hir::Stmt> {
  auto validate = ValidateAssignableImpl(proc.Owner(), true, as.left());
  if (!validate) return std::unexpected(std::move(validate.error()));
  auto type_or = proc.Owner().InternType(*as.type, span);
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
                .name = std::string{kHeldValueName},
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
                    .compound_op = std::nullopt,
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

  if (const auto* repeated =
          std::get_if<hir::RepeatedEventControl>(&*control)) {
    // LRM 9.4.5: the count is how many occurrences the assignment waits out, so
    // a count of none reaches the assignment where the statement stands.
    const hir::StmtId nothing = plain(hir::EmptyStmt{});
    const hir::StmtId wait = plain(
        hir::TimedStmt{
            .timing = std::visit(
                [](auto entry) { return hir::TimingControl{std::move(entry)}; },
                repeated->event),
            .stmt = nothing});
    statements.push_back(
        plain(hir::RepeatStmt{.count = repeated->count, .body = wait}));
    statements.push_back(assign);
  } else {
    statements.push_back(plain(
        hir::TimedStmt{.timing = AsStatementTiming(*control), .stmt = assign}));
  }

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
  if (!AsWatchedStorage(*expr_or).has_value()) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedStatementForm,
        "event trigger target must be a plain named-event reference");
  }
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

// LRM 9.4.3 `wait (cond) body`. The wait re-evaluates when any cell the
// condition reads changes, so its sensitivity is that condition's own read set
// -- narrower than the enclosing body's, which is why it is analyzed here
// rather than inherited.
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
  const auto& reads =
      proc.Owner().Sensitivity().AnalyzeReads(w.cond, proc.ContainingSymbol());
  auto sensitivity = proc.Owner().TranslateSensitivityReads(reads, frame);
  if (!sensitivity) return std::unexpected(std::move(sensitivity.error()));
  return hir::Stmt{
      .label = std::nullopt,
      .data =
          hir::WaitStmt{
              .cond = cond_id,
              .body = body_id,
              .sensitivity_list = *std::move(sensitivity)},
      .span = span};
}

}  // namespace lyra::lowering::ast_to_hir
