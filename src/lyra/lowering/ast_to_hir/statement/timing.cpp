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
  // Faithful record of SV: every leaf carries the trigger's edge identifier.
  // Whether the runtime can act on it directly (single leaf, LSB-reduce) or
  // needs a snapshot + re-eval wrapper (compound) is a HIR -> MIR decision.
  auto sensitivity_list =
      proc.Owner().TranslateSensitivityReads(reads, frame, edge_kind);
  if (!sensitivity_list) {
    return std::unexpected(std::move(sensitivity_list.error()));
  }

  return hir::EventTrigger{
      .signal = frame.Exprs().Add(*std::move(expr_or)),
      .edge = edge_kind,
      .sensitivity_list = *std::move(sensitivity_list),
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
    case slang::ast::TimingControlKind::ImplicitEvent: {
      const auto& reads = proc.Owner().Sensitivity().AnalyzeReads(
          controlled, proc.ContainingSymbol());
      auto sensitivity = proc.Owner().TranslateSensitivityReads(
          reads, frame, support::EventEdge::kAnyChange);
      if (!sensitivity) return std::unexpected(std::move(sensitivity.error()));
      return hir::TimingControl{hir::ImplicitEventControl{
          .sensitivity_list = *std::move(sensitivity)}};
    }
    case slang::ast::TimingControlKind::RepeatedEvent:
      // LRM A.6.5: a repeat event control is only ever an intra-assignment
      // control -- what prefixes a statement is a delay, an event control, or a
      // cycle delay -- and the intra-assignment form is expanded into its
      // repeat loop before it reaches here.
      throw InternalError(
          "LowerTimingControl: a repeated event control reached statement "
          "timing, where the grammar does not put one");
    default:
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedTimingControlKind,
          "this timing control kind is not yet supported");
  }
}

// The name a held right-hand side carries. LRM 9.4.5 gives it no name of its
// own, so one that cannot collide with a design's is minted here.
constexpr std::string_view kHeldValueName = "_lyra_intra_assign";

}  // namespace

auto LowerIntraAssignmentStmt(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::AssignmentExpression& as,
    const slang::ast::Statement& controlled, diag::SourceSpan span)
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
                    .kind = hir::BlockingAssign{},
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

  const auto* repeated =
      as.timingControl->kind == slang::ast::TimingControlKind::RepeatedEvent
          ? &as.timingControl->as<slang::ast::RepeatedEventControl>()
          : nullptr;
  auto timing = LowerTimingControl(
      proc, inner, repeated != nullptr ? repeated->event : *as.timingControl,
      controlled, span);
  if (!timing) return std::unexpected(std::move(timing.error()));

  if (repeated == nullptr) {
    statements.push_back(
        plain(hir::TimedStmt{.timing = *std::move(timing), .stmt = assign}));
  } else {
    auto count_or = proc.LowerExpr(repeated->expr, inner);
    if (!count_or) return std::unexpected(std::move(count_or.error()));
    const hir::StmtId nothing = plain(hir::EmptyStmt{});
    const hir::StmtId wait =
        plain(hir::TimedStmt{.timing = *std::move(timing), .stmt = nothing});
    statements.push_back(plain(
        hir::RepeatStmt{
            .count = inner.Exprs().Add(*std::move(count_or)), .body = wait}));
    statements.push_back(assign);
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
  auto sensitivity = proc.Owner().TranslateSensitivityReads(
      reads, frame, support::EventEdge::kAnyChange);
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
