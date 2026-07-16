#include "lyra/lowering/ast_to_hir/statement/timing.hpp"

#include <expected>
#include <optional>
#include <utility>
#include <variant>
#include <vector>

#include <slang/ast/Statement.h>
#include <slang/ast/TimingControl.h>
#include <slang/ast/statements/MiscStatements.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/hir/value_ref.hpp"
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

auto LowerSignalEventTrigger(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::SignalEventControl& sig, diag::SourceSpan span)
    -> diag::Result<hir::EventTrigger> {
  if (sig.iffCondition != nullptr) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedEventTriggerForm,
        "`iff` qualifier on event control is not yet supported");
  }

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

  const auto& reads = proc.Owner().Sensitivity().AnalyzeReads(
      sig.expr, proc.ContainingSymbol());
  auto sensitivity_list = proc.Owner().TranslateSensitivityReads(reads, frame);
  // Faithful record of SV: every leaf carries the trigger's edge identifier.
  // Whether the runtime can act on it directly (single leaf, LSB-reduce) or
  // needs a snapshot + re-eval wrapper (compound) is a HIR -> MIR decision.
  for (auto& leaf : sensitivity_list) {
    leaf.edge_kind = edge_kind;
  }

  return hir::EventTrigger{
      .signal = frame.Exprs().Add(*std::move(expr_or)),
      .edge = edge_kind,
      .sensitivity_list = std::move(sensitivity_list),
  };
}

// LRM 15.5.2 `@e;` on a named event. Distinguished from value-change `@(sig)`
// by the controlled expression's type. Identity-only -- no edge polarity
// applies, so reject any edge qualifier here.
auto LowerNamedEventControl(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::SignalEventControl& sig, diag::SourceSpan span)
    -> diag::Result<hir::NamedEventControl> {
  if (sig.iffCondition != nullptr) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedEventTriggerForm,
        "`iff` qualifier on event control is not yet supported");
  }
  if (sig.edge != slang::ast::EdgeKind::None) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedEventTriggerForm,
        "edge specifier is not valid on a named event");
  }

  auto expr_or = proc.LowerExpr(sig.expr, frame);
  if (!expr_or) return std::unexpected(std::move(expr_or.error()));

  const auto* primary = std::get_if<hir::PrimaryExpr>(&expr_or->data);
  if (primary == nullptr ||
      !(std::holds_alternative<hir::DirectMemberRef>(primary->data) ||
        std::holds_alternative<hir::RoutedRef>(primary->data))) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedEventTriggerForm,
        "named event reference must be a plain structural variable");
  }
  return hir::NamedEventControl{
      .event = frame.Exprs().Add(*std::move(expr_or)),
  };
}

auto LowerTimingControl(
    ProcessLowerer& proc, WalkFrame frame, const slang::ast::TimingControl& tc,
    diag::SourceSpan span) -> diag::Result<hir::TimingControl> {
  switch (tc.kind) {
    case slang::ast::TimingControlKind::Delay: {
      const auto& delay = tc.as<slang::ast::DelayControl>();
      auto duration = proc.LowerExpr(delay.expr, frame);
      if (!duration) return std::unexpected(std::move(duration.error()));
      return hir::TimingControl{hir::DelayControl{
          .duration = frame.Exprs().Add(*std::move(duration))}};
    }
    case slang::ast::TimingControlKind::SignalEvent: {
      const auto& sig = tc.as<slang::ast::SignalEventControl>();
      // Named events (LRM 15.5.2) and value-change events (LRM 9.4.2) share
      // slang's SignalEventControl shape; distinguish by the controlled
      // expression's type.
      if (sig.expr.type->isEvent()) {
        auto nec_or = LowerNamedEventControl(proc, frame, sig, span);
        if (!nec_or) return std::unexpected(std::move(nec_or.error()));
        return hir::TimingControl{*std::move(nec_or)};
      }
      auto trigger_or = LowerSignalEventTrigger(proc, frame, sig, span);
      if (!trigger_or) return std::unexpected(std::move(trigger_or.error()));
      return hir::TimingControl{
          hir::EventControl{.triggers = {*std::move(trigger_or)}}};
    }
    case slang::ast::TimingControlKind::EventList: {
      const auto& list = tc.as<slang::ast::EventListControl>();
      std::vector<hir::EventTrigger> triggers;
      triggers.reserve(list.events.size());
      for (const auto* event : list.events) {
        if (event->kind != slang::ast::TimingControlKind::SignalEvent) {
          return diag::Fail(
              span, diag::DiagCode::kUnsupportedTimingControlKind,
              "event list entries must be signal events; nested timing "
              "controls are not yet supported");
        }
        const auto& sig = event->as<slang::ast::SignalEventControl>();
        auto trigger_or = LowerSignalEventTrigger(proc, frame, sig, span);
        if (!trigger_or) return std::unexpected(std::move(trigger_or.error()));
        triggers.push_back(*std::move(trigger_or));
      }
      return hir::TimingControl{
          hir::EventControl{.triggers = std::move(triggers)}};
    }
    case slang::ast::TimingControlKind::ImplicitEvent:
      return hir::TimingControl{hir::ImplicitEventControl{}};
    case slang::ast::TimingControlKind::RepeatedEvent:
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedTimingControlKind,
          "repeated event control (`repeat (N) @(...)`) is not yet supported");
    default:
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedTimingControlKind,
          "this timing control kind is not yet supported");
  }
}

}  // namespace

auto LowerTimedStmt(
    ProcessLowerer& proc, WalkFrame frame, const slang::ast::TimedStatement& ts,
    diag::SourceSpan span) -> diag::Result<hir::Stmt> {
  auto timing = LowerTimingControl(proc, frame, ts.timing, span);
  if (!timing) return std::unexpected(std::move(timing.error()));
  auto inner_stmt = proc.LowerStmt(ts.stmt, frame);
  if (!inner_stmt) return std::unexpected(std::move(inner_stmt.error()));
  const hir::StmtId inner_id =
      frame.current_procedural_body->stmts.Add(*std::move(inner_stmt));
  // LRM 9.4.2.2: `@*` watches every read in the controlled body. Inferred after
  // the body lowers so a read's cross-unit reference is already resolved when
  // its subscription is built.
  if (auto* ie = std::get_if<hir::ImplicitEventControl>(&*timing)) {
    const auto& reads = proc.Owner().Sensitivity().AnalyzeReads(
        ts.stmt, proc.ContainingSymbol());
    ie->sensitivity_list = proc.Owner().TranslateSensitivityReads(reads, frame);
  }
  return hir::Stmt{
      .label = std::nullopt,
      .data = hir::TimedStmt{.timing = *std::move(timing), .stmt = inner_id},
      .span = span};
}

// LRM 15.5.1 `-> e;`. Source-aligned with slang's EventTriggerStatement. The
// `->>` non-blocking form and any delay-or-event-control prefix are deferred.
auto LowerEventTriggerStmt(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::EventTriggerStatement& et, diag::SourceSpan span)
    -> diag::Result<hir::Stmt> {
  if (et.isNonBlocking) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedStatementForm,
        "non-blocking event trigger `->>` is not yet supported");
  }
  if (et.timing != nullptr) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedStatementForm,
        "delayed event trigger (with intra-trigger timing control) is not yet "
        "supported");
  }
  auto expr_or = proc.LowerExpr(et.target, frame);
  if (!expr_or) return std::unexpected(std::move(expr_or.error()));
  const auto* primary = std::get_if<hir::PrimaryExpr>(&expr_or->data);
  if (primary == nullptr ||
      !(std::holds_alternative<hir::DirectMemberRef>(primary->data) ||
        std::holds_alternative<hir::RoutedRef>(primary->data))) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedStatementForm,
        "event trigger target must be a plain named-event reference");
  }
  return hir::Stmt{
      .label = std::nullopt,
      .data =
          hir::EventTriggerStmt{
              .event = frame.Exprs().Add(*std::move(expr_or)),
          },
      .span = span};
}

// LRM 9.4.3 `wait (cond) body`. Sensitivity is precomputed by driving slang's
// flow analysis on `w.cond` via a per-wait `DefaultDFA` run upstream; the
// result is looked up here keyed by the cond expression.
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
  return hir::Stmt{
      .label = std::nullopt,
      .data =
          hir::WaitStmt{
              .cond = cond_id,
              .body = body_id,
              .sensitivity_list = std::move(sensitivity)},
      .span = span};
}

}  // namespace lyra::lowering::ast_to_hir
