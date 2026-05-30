#include "lyra/lowering/ast_to_hir/statement/lower.hpp"

#include <expected>
#include <optional>
#include <utility>
#include <vector>

#include <slang/ast/Expression.h>
#include <slang/ast/Statement.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/TimingControl.h>
#include <slang/ast/statements/ConditionalStatements.h>
#include <slang/ast/statements/LoopStatements.h>
#include <slang/ast/statements/MiscStatements.h>
#include <slang/ast/symbols/VariableSymbols.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/kind.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/ast_to_hir/expression/lower.hpp"
#include "lyra/lowering/ast_to_hir/facts.hpp"
#include "lyra/lowering/ast_to_hir/sensitivity.hpp"
#include "lyra/lowering/ast_to_hir/state.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

auto LowerUniquePriorityCheck(slang::ast::UniquePriorityCheck check)
    -> std::optional<hir::UniquePriorityCheck> {
  switch (check) {
    case slang::ast::UniquePriorityCheck::None:
      return std::nullopt;
    case slang::ast::UniquePriorityCheck::Unique:
      return hir::UniquePriorityCheck::kUnique;
    case slang::ast::UniquePriorityCheck::Unique0:
      return hir::UniquePriorityCheck::kUnique0;
    case slang::ast::UniquePriorityCheck::Priority:
      return hir::UniquePriorityCheck::kPriority;
  }
  throw InternalError(
      "LowerUniquePriorityCheck: unknown slang UniquePriorityCheck value");
}

auto LowerEventEdge(slang::ast::EdgeKind kind) -> hir::EventEdge {
  switch (kind) {
    case slang::ast::EdgeKind::None:
      return hir::EventEdge::kAnyChange;
    case slang::ast::EdgeKind::PosEdge:
      return hir::EventEdge::kPosedge;
    case slang::ast::EdgeKind::NegEdge:
      return hir::EventEdge::kNegedge;
    case slang::ast::EdgeKind::BothEdges:
      return hir::EventEdge::kBothEdges;
  }
  throw InternalError("LowerEventEdge: unknown slang EdgeKind value");
}

auto LowerSignalEventTrigger(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ProcessLoweringState& proc_state, const ScopeStack& stack,
    const slang::ast::SignalEventControl& sig, diag::SourceSpan span)
    -> diag::Result<hir::EventTrigger> {
  if (sig.iffCondition != nullptr) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedEventTriggerForm,
        "`iff` qualifier on event control is not yet supported",
        diag::UnsupportedCategory::kFeature);
  }

  auto expr_or =
      LowerProcExpr(unit_facts, unit_state, proc_state, stack, sig.expr);
  if (!expr_or) return std::unexpected(std::move(expr_or.error()));

  const auto* primary = std::get_if<hir::PrimaryExpr>(&expr_or->data);
  if (primary == nullptr ||
      !std::holds_alternative<hir::StructuralVarRef>(primary->data)) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedEventTriggerForm,
        "event trigger must be a plain structural variable reference; "
        "bit-selects, member access, and hierarchical refs are "
        "not yet supported",
        diag::UnsupportedCategory::kFeature);
  }
  if (!unit_state.GetType(expr_or->type).IsPackedArray()) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedEventTriggerForm,
        "event trigger must reference an integral signal; non-integral "
        "trigger types are not yet supported",
        diag::UnsupportedCategory::kFeature);
  }

  return hir::EventTrigger{
      .signal = proc_state.AddExpr(*std::move(expr_or)),
      .edge = LowerEventEdge(sig.edge),
  };
}

// LRM 15.5.2 `@e;` on a named event. Distinguished from value-change `@(sig)`
// by the controlled expression's type. Identity-only -- no edge polarity
// applies, so reject any edge qualifier here.
auto LowerNamedEventControl(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ProcessLoweringState& proc_state, const ScopeStack& stack,
    const slang::ast::SignalEventControl& sig, diag::SourceSpan span)
    -> diag::Result<hir::NamedEventControl> {
  if (sig.iffCondition != nullptr) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedEventTriggerForm,
        "`iff` qualifier on event control is not yet supported",
        diag::UnsupportedCategory::kFeature);
  }
  if (sig.edge != slang::ast::EdgeKind::None) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedEventTriggerForm,
        "edge specifier is not valid on a named event",
        diag::UnsupportedCategory::kFeature);
  }

  auto expr_or =
      LowerProcExpr(unit_facts, unit_state, proc_state, stack, sig.expr);
  if (!expr_or) return std::unexpected(std::move(expr_or.error()));

  const auto* primary = std::get_if<hir::PrimaryExpr>(&expr_or->data);
  if (primary == nullptr ||
      !std::holds_alternative<hir::StructuralVarRef>(primary->data)) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedEventTriggerForm,
        "named event reference must be a plain structural variable",
        diag::UnsupportedCategory::kFeature);
  }
  return hir::NamedEventControl{
      .event = proc_state.AddExpr(*std::move(expr_or)),
  };
}

auto LowerTimingControl(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ProcessLoweringState& proc_state, const ScopeStack& stack,
    const slang::ast::TimingControl& tc, diag::SourceSpan span)
    -> diag::Result<hir::TimingControl> {
  switch (tc.kind) {
    case slang::ast::TimingControlKind::Delay: {
      const auto& delay = tc.as<slang::ast::DelayControl>();
      auto duration =
          LowerProcExpr(unit_facts, unit_state, proc_state, stack, delay.expr);
      if (!duration) return std::unexpected(std::move(duration.error()));
      return hir::TimingControl{hir::DelayControl{
          .duration = proc_state.AddExpr(*std::move(duration))}};
    }
    case slang::ast::TimingControlKind::SignalEvent: {
      const auto& sig = tc.as<slang::ast::SignalEventControl>();
      // Named events (LRM 15.5.2) and value-change events (LRM 9.4.2) share
      // slang's SignalEventControl shape; distinguish by the controlled
      // expression's type.
      if (sig.expr.type->isEvent()) {
        auto nec_or = LowerNamedEventControl(
            unit_facts, unit_state, proc_state, stack, sig, span);
        if (!nec_or) return std::unexpected(std::move(nec_or.error()));
        return hir::TimingControl{*std::move(nec_or)};
      }
      auto trigger_or = LowerSignalEventTrigger(
          unit_facts, unit_state, proc_state, stack, sig, span);
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
          return diag::Unsupported(
              span, diag::DiagCode::kUnsupportedTimingControlKind,
              "event list entries must be signal events; nested timing "
              "controls are not yet supported",
              diag::UnsupportedCategory::kFeature);
        }
        const auto& sig = event->as<slang::ast::SignalEventControl>();
        auto trigger_or = LowerSignalEventTrigger(
            unit_facts, unit_state, proc_state, stack, sig, span);
        if (!trigger_or) return std::unexpected(std::move(trigger_or.error()));
        triggers.push_back(*std::move(trigger_or));
      }
      return hir::TimingControl{
          hir::EventControl{.triggers = std::move(triggers)}};
    }
    case slang::ast::TimingControlKind::ImplicitEvent:
      return hir::TimingControl{hir::ImplicitEventControl{}};
    case slang::ast::TimingControlKind::RepeatedEvent:
      return diag::Unsupported(
          span, diag::DiagCode::kUnsupportedTimingControlKind,
          "repeated event control (`repeat (N) @(...)`) is not yet supported",
          diag::UnsupportedCategory::kFeature);
    default:
      return diag::Unsupported(
          span, diag::DiagCode::kUnsupportedTimingControlKind,
          "this timing control kind is not yet supported",
          diag::UnsupportedCategory::kFeature);
  }
}

auto LowerEmptyStmt(diag::SourceSpan span) -> diag::Result<hir::Stmt> {
  return hir::Stmt{
      .label = std::nullopt, .data = hir::EmptyStmt{}, .span = span};
}

auto LowerTimedStmt(
    const UnitLoweringFacts& unit_facts, ProcessLoweringState& proc_state,
    ScopeLoweringState& scope_state, ScopeStack& stack,
    const slang::ast::TimedStatement& ts, diag::SourceSpan span)
    -> diag::Result<hir::Stmt> {
  auto timing = LowerTimingControl(
      unit_facts, scope_state.UnitState(), proc_state, stack, ts.timing, span);
  if (!timing) return std::unexpected(std::move(timing.error()));
  // LRM 9.4.2.2: `@*` carries its sensitivity from a slang side-channel
  // keyed by this TimedStatement. The TimingControl shell came back empty
  // from LowerTimingControl; fill it in here where we still have the
  // parent statement pointer.
  if (auto* ie = std::get_if<hir::ImplicitEventControl>(&*timing)) {
    if (const auto* reads = unit_facts.SensitivityReads().Lookup(ts);
        reads != nullptr) {
      ie->sensitivity_list =
          TranslateSensitivityReads(*reads, scope_state.UnitState(), stack);
    }
  }
  auto inner_stmt =
      LowerStatement(unit_facts, proc_state, scope_state, stack, ts.stmt);
  if (!inner_stmt) return std::unexpected(std::move(inner_stmt.error()));
  const hir::StmtId inner_id = proc_state.AddStmt(*std::move(inner_stmt));
  return hir::Stmt{
      .label = std::nullopt,
      .data = hir::TimedStmt{.timing = *std::move(timing), .stmt = inner_id},
      .span = span};
}

auto LowerStatementListStmt(
    const UnitLoweringFacts& unit_facts, ProcessLoweringState& proc_state,
    ScopeLoweringState& scope_state, ScopeStack& stack,
    const slang::ast::StatementList& list, diag::SourceSpan span)
    -> diag::Result<hir::Stmt> {
  std::vector<hir::StmtId> kids;
  kids.reserve(list.list.size());
  for (const auto* child : list.list) {
    auto child_stmt =
        LowerStatement(unit_facts, proc_state, scope_state, stack, *child);
    if (!child_stmt) return std::unexpected(std::move(child_stmt.error()));
    kids.push_back(proc_state.AddStmt(*std::move(child_stmt)));
  }
  return hir::Stmt{
      .label = std::nullopt,
      .data = hir::BlockStmt{.statements = std::move(kids)},
      .span = span};
}

auto LowerBlockStmt(
    const UnitLoweringFacts& unit_facts, ProcessLoweringState& proc_state,
    ScopeLoweringState& scope_state, ScopeStack& stack,
    const slang::ast::BlockStatement& block, diag::SourceSpan span)
    -> diag::Result<hir::Stmt> {
  std::vector<hir::StmtId> kids;
  if (block.body.kind == slang::ast::StatementKind::List) {
    const auto& list = block.body.as<slang::ast::StatementList>();
    kids.reserve(list.list.size());
    for (const auto* child : list.list) {
      auto child_stmt =
          LowerStatement(unit_facts, proc_state, scope_state, stack, *child);
      if (!child_stmt) return std::unexpected(std::move(child_stmt.error()));
      kids.push_back(proc_state.AddStmt(*std::move(child_stmt)));
    }
  } else {
    auto child_stmt =
        LowerStatement(unit_facts, proc_state, scope_state, stack, block.body);
    if (!child_stmt) return std::unexpected(std::move(child_stmt.error()));
    kids.push_back(proc_state.AddStmt(*std::move(child_stmt)));
  }
  return hir::Stmt{
      .label = std::nullopt,
      .data = hir::BlockStmt{.statements = std::move(kids)},
      .span = span};
}

auto LowerVariableDeclStmt(
    const UnitLoweringFacts& unit_facts, ProcessLoweringState& proc_state,
    ScopeLoweringState& scope_state, const ScopeStack& stack,
    const slang::ast::VariableDeclStatement& vd, diag::SourceSpan span)
    -> diag::Result<hir::Stmt> {
  const auto& mapper = unit_facts.SourceMapper();
  const auto& sym = vd.symbol;
  auto type_id_or = scope_state.UnitState().GetTypeId(
      sym.getType(), mapper.PointSpanOf(sym.location));
  if (!type_id_or) return std::unexpected(std::move(type_id_or.error()));
  const auto local_id = proc_state.AddProceduralVar(sym, *type_id_or);
  std::optional<hir::ExprId> init_id;
  if (const auto* init_expr = sym.getInitializer()) {
    auto init_or = LowerProcExpr(
        unit_facts, scope_state.UnitState(), proc_state, stack, *init_expr);
    if (!init_or) return std::unexpected(std::move(init_or.error()));
    init_id = proc_state.AddExpr(*std::move(init_or));
  }
  return hir::Stmt{
      .label = std::nullopt,
      .data = hir::VarDeclStmt{.var = local_id, .init = init_id},
      .span = span};
}

auto LowerExpressionStmt(
    const UnitLoweringFacts& unit_facts, ProcessLoweringState& proc_state,
    ScopeLoweringState& scope_state, const ScopeStack& stack,
    const slang::ast::ExpressionStatement& es, diag::SourceSpan span)
    -> diag::Result<hir::Stmt> {
  auto expr = LowerProcExpr(
      unit_facts, scope_state.UnitState(), proc_state, stack, es.expr);
  if (!expr) return std::unexpected(std::move(expr.error()));
  const hir::ExprId id = proc_state.AddExpr(*std::move(expr));
  return hir::Stmt{
      .label = std::nullopt, .data = hir::ExprStmt{.expr = id}, .span = span};
}

auto LowerForLoopStmt(
    const UnitLoweringFacts& unit_facts, ProcessLoweringState& proc_state,
    ScopeLoweringState& scope_state, ScopeStack& stack,
    const slang::ast::ForLoopStatement& fs, diag::SourceSpan span)
    -> diag::Result<hir::Stmt> {
  // slang elaborates `for (int i = 0; ...)` as an independent preceding
  // VariableDeclStatement plus a ForLoopStatement whose `initializers` is
  // empty and `loopVars` only points at the already-declared symbol. The
  // preceding VarDeclStatement carries the initializer, so loopVars is
  // informational only and is ignored here.
  std::vector<hir::ForInit> hir_init;
  hir_init.reserve(fs.initializers.size());
  for (const auto* init_expr : fs.initializers) {
    auto init_or = LowerProcExpr(
        unit_facts, scope_state.UnitState(), proc_state, stack, *init_expr);
    if (!init_or) return std::unexpected(std::move(init_or.error()));
    hir_init.emplace_back(
        hir::ForInitExpr{.expr = proc_state.AddExpr(*std::move(init_or))});
  }
  std::optional<hir::ExprId> cond_id;
  if (fs.stopExpr != nullptr) {
    auto cond_or = LowerProcExpr(
        unit_facts, scope_state.UnitState(), proc_state, stack, *fs.stopExpr);
    if (!cond_or) return std::unexpected(std::move(cond_or.error()));
    cond_id = proc_state.AddExpr(*std::move(cond_or));
  }
  std::vector<hir::ExprId> step_ids;
  step_ids.reserve(fs.steps.size());
  for (const auto* step_expr : fs.steps) {
    auto step_or = LowerProcExpr(
        unit_facts, scope_state.UnitState(), proc_state, stack, *step_expr);
    if (!step_or) return std::unexpected(std::move(step_or.error()));
    step_ids.push_back(proc_state.AddExpr(*std::move(step_or)));
  }
  auto body_stmt =
      LowerStatement(unit_facts, proc_state, scope_state, stack, fs.body);
  if (!body_stmt) return std::unexpected(std::move(body_stmt.error()));
  const hir::StmtId body_id = proc_state.AddStmt(*std::move(body_stmt));
  return hir::Stmt{
      .label = std::nullopt,
      .data =
          hir::ForStmt{
              .init = std::move(hir_init),
              .condition = cond_id,
              .step = std::move(step_ids),
              .body = body_id},
      .span = span};
}

auto LowerWhileLoopStmt(
    const UnitLoweringFacts& unit_facts, ProcessLoweringState& proc_state,
    ScopeLoweringState& scope_state, ScopeStack& stack,
    const slang::ast::WhileLoopStatement& ws, diag::SourceSpan span)
    -> diag::Result<hir::Stmt> {
  auto cond_or = LowerProcExpr(
      unit_facts, scope_state.UnitState(), proc_state, stack, ws.cond);
  if (!cond_or) return std::unexpected(std::move(cond_or.error()));
  const hir::ExprId cond_id = proc_state.AddExpr(*std::move(cond_or));
  auto body_or =
      LowerStatement(unit_facts, proc_state, scope_state, stack, ws.body);
  if (!body_or) return std::unexpected(std::move(body_or.error()));
  const hir::StmtId body_id = proc_state.AddStmt(*std::move(body_or));
  return hir::Stmt{
      .label = std::nullopt,
      .data = hir::WhileStmt{.condition = cond_id, .body = body_id},
      .span = span};
}

auto LowerRepeatLoopStmt(
    const UnitLoweringFacts& unit_facts, ProcessLoweringState& proc_state,
    ScopeLoweringState& scope_state, ScopeStack& stack,
    const slang::ast::RepeatLoopStatement& rs, diag::SourceSpan span)
    -> diag::Result<hir::Stmt> {
  auto count_or = LowerProcExpr(
      unit_facts, scope_state.UnitState(), proc_state, stack, rs.count);
  if (!count_or) return std::unexpected(std::move(count_or.error()));
  const hir::ExprId count_id = proc_state.AddExpr(*std::move(count_or));
  auto body_or =
      LowerStatement(unit_facts, proc_state, scope_state, stack, rs.body);
  if (!body_or) return std::unexpected(std::move(body_or.error()));
  const hir::StmtId body_id = proc_state.AddStmt(*std::move(body_or));
  return hir::Stmt{
      .label = std::nullopt,
      .data = hir::RepeatStmt{.count = count_id, .body = body_id},
      .span = span};
}

auto LowerDoWhileLoopStmt(
    const UnitLoweringFacts& unit_facts, ProcessLoweringState& proc_state,
    ScopeLoweringState& scope_state, ScopeStack& stack,
    const slang::ast::DoWhileLoopStatement& ds, diag::SourceSpan span)
    -> diag::Result<hir::Stmt> {
  auto body_or =
      LowerStatement(unit_facts, proc_state, scope_state, stack, ds.body);
  if (!body_or) return std::unexpected(std::move(body_or.error()));
  const hir::StmtId body_id = proc_state.AddStmt(*std::move(body_or));
  auto cond_or = LowerProcExpr(
      unit_facts, scope_state.UnitState(), proc_state, stack, ds.cond);
  if (!cond_or) return std::unexpected(std::move(cond_or.error()));
  const hir::ExprId cond_id = proc_state.AddExpr(*std::move(cond_or));
  return hir::Stmt{
      .label = std::nullopt,
      .data = hir::DoWhileStmt{.condition = cond_id, .body = body_id},
      .span = span};
}

auto LowerForeverLoopStmt(
    const UnitLoweringFacts& unit_facts, ProcessLoweringState& proc_state,
    ScopeLoweringState& scope_state, ScopeStack& stack,
    const slang::ast::ForeverLoopStatement& fs, diag::SourceSpan span)
    -> diag::Result<hir::Stmt> {
  auto body_or =
      LowerStatement(unit_facts, proc_state, scope_state, stack, fs.body);
  if (!body_or) return std::unexpected(std::move(body_or.error()));
  const hir::StmtId body_id = proc_state.AddStmt(*std::move(body_or));
  return hir::Stmt{
      .label = std::nullopt,
      .data = hir::ForeverStmt{.body = body_id},
      .span = span};
}

auto LowerBreakStmt(diag::SourceSpan span) -> diag::Result<hir::Stmt> {
  return hir::Stmt{
      .label = std::nullopt, .data = hir::BreakStmt{}, .span = span};
}

auto LowerContinueStmt(diag::SourceSpan span) -> diag::Result<hir::Stmt> {
  return hir::Stmt{
      .label = std::nullopt, .data = hir::ContinueStmt{}, .span = span};
}

auto LowerCaseStmt(
    const UnitLoweringFacts& unit_facts, ProcessLoweringState& proc_state,
    ScopeLoweringState& scope_state, ScopeStack& stack,
    const slang::ast::CaseStatement& cs, diag::SourceSpan span)
    -> diag::Result<hir::Stmt> {
  if (cs.condition != slang::ast::CaseStatementCondition::Normal) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedStatementForm,
        "casez/casex/case-inside are not yet supported",
        diag::UnsupportedCategory::kFeature);
  }
  const auto case_check = LowerUniquePriorityCheck(cs.check);
  auto cond_expr = LowerProcExpr(
      unit_facts, scope_state.UnitState(), proc_state, stack, cs.expr);
  if (!cond_expr) return std::unexpected(std::move(cond_expr.error()));
  const hir::ExprId cond_id = proc_state.AddExpr(*std::move(cond_expr));
  std::vector<hir::CaseItem> items;
  items.reserve(cs.items.size());
  for (const auto& item : cs.items) {
    std::vector<hir::ExprId> label_ids;
    label_ids.reserve(item.expressions.size());
    for (const auto* label_expr : item.expressions) {
      auto label_or = LowerProcExpr(
          unit_facts, scope_state.UnitState(), proc_state, stack, *label_expr);
      if (!label_or) return std::unexpected(std::move(label_or.error()));
      label_ids.push_back(proc_state.AddExpr(*std::move(label_or)));
    }
    auto item_stmt =
        LowerStatement(unit_facts, proc_state, scope_state, stack, *item.stmt);
    if (!item_stmt) return std::unexpected(std::move(item_stmt.error()));
    const hir::StmtId item_id = proc_state.AddStmt(*std::move(item_stmt));
    items.push_back(
        hir::CaseItem{.labels = std::move(label_ids), .stmt = item_id});
  }
  std::optional<hir::StmtId> default_id;
  if (cs.defaultCase != nullptr) {
    auto default_stmt = LowerStatement(
        unit_facts, proc_state, scope_state, stack, *cs.defaultCase);
    if (!default_stmt) return std::unexpected(std::move(default_stmt.error()));
    default_id = proc_state.AddStmt(*std::move(default_stmt));
  }
  return hir::Stmt{
      .label = std::nullopt,
      .data =
          hir::CaseStmt{
              .condition = cond_id,
              .items = std::move(items),
              .default_stmt = default_id,
              .check = case_check},
      .span = span};
}

auto LowerConditionalStmt(
    const UnitLoweringFacts& unit_facts, ProcessLoweringState& proc_state,
    ScopeLoweringState& scope_state, ScopeStack& stack,
    const slang::ast::ConditionalStatement& cs, diag::SourceSpan span)
    -> diag::Result<hir::Stmt> {
  const auto if_check = LowerUniquePriorityCheck(cs.check);
  if (cs.conditions.size() != 1) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedStatementForm,
        "multi-condition if expressions are not yet supported",
        diag::UnsupportedCategory::kFeature);
  }
  const auto& cond = cs.conditions.front();
  if (cond.pattern != nullptr) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedStatementForm,
        "pattern matching in if conditions is not yet supported",
        diag::UnsupportedCategory::kFeature);
  }
  auto cond_expr = LowerProcExpr(
      unit_facts, scope_state.UnitState(), proc_state, stack, *cond.expr);
  if (!cond_expr) return std::unexpected(std::move(cond_expr.error()));
  const hir::ExprId cond_id = proc_state.AddExpr(*std::move(cond_expr));
  auto then_stmt =
      LowerStatement(unit_facts, proc_state, scope_state, stack, cs.ifTrue);
  if (!then_stmt) return std::unexpected(std::move(then_stmt.error()));
  const hir::StmtId then_id = proc_state.AddStmt(*std::move(then_stmt));
  std::optional<hir::StmtId> else_id;
  if (cs.ifFalse != nullptr) {
    auto else_stmt =
        LowerStatement(unit_facts, proc_state, scope_state, stack, *cs.ifFalse);
    if (!else_stmt) return std::unexpected(std::move(else_stmt.error()));
    else_id = proc_state.AddStmt(*std::move(else_stmt));
  }
  return hir::Stmt{
      .label = std::nullopt,
      .data =
          hir::IfStmt{
              .condition = cond_id,
              .then_stmt = then_id,
              .else_stmt = else_id,
              .check = if_check},
      .span = span};
}

}  // namespace

// LRM 15.5.1 `-> e;`. Source-aligned with slang's EventTriggerStatement. The
// `->>` non-blocking form and any delay-or-event-control prefix are deferred.
auto LowerEventTriggerStmt(
    const UnitLoweringFacts& unit_facts, ProcessLoweringState& proc_state,
    ScopeLoweringState& scope_state, const ScopeStack& stack,
    const slang::ast::EventTriggerStatement& et, diag::SourceSpan span)
    -> diag::Result<hir::Stmt> {
  if (et.isNonBlocking) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedStatementForm,
        "non-blocking event trigger `->>` is not yet supported",
        diag::UnsupportedCategory::kFeature);
  }
  if (et.timing != nullptr) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedStatementForm,
        "delayed event trigger (with intra-trigger timing control) is not yet "
        "supported",
        diag::UnsupportedCategory::kFeature);
  }
  auto expr_or = LowerProcExpr(
      unit_facts, scope_state.UnitState(), proc_state, stack, et.target);
  if (!expr_or) return std::unexpected(std::move(expr_or.error()));
  const auto* primary = std::get_if<hir::PrimaryExpr>(&expr_or->data);
  if (primary == nullptr ||
      !std::holds_alternative<hir::StructuralVarRef>(primary->data)) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedStatementForm,
        "event trigger target must be a plain named-event reference",
        diag::UnsupportedCategory::kFeature);
  }
  return hir::Stmt{
      .label = std::nullopt,
      .data =
          hir::EventTriggerStmt{
              .event = proc_state.AddExpr(*std::move(expr_or)),
          },
      .span = span};
}

// LRM 9.4.3 `wait (cond) body`. Sensitivity is precomputed by driving slang's
// flow analysis on `w.cond` via a per-wait `DefaultDFA` run inside
// `BuildSensitivityReadStore` (see `docs/decisions/read-set-inference.md`).
// We look up the result here keyed by the cond expression.
auto LowerWaitStmt(
    const UnitLoweringFacts& unit_facts, ProcessLoweringState& proc_state,
    ScopeLoweringState& scope_state, ScopeStack& stack,
    const slang::ast::WaitStatement& w, diag::SourceSpan span)
    -> diag::Result<hir::Stmt> {
  auto cond_or = LowerProcExpr(
      unit_facts, scope_state.UnitState(), proc_state, stack, w.cond);
  if (!cond_or) return std::unexpected(std::move(cond_or.error()));
  const hir::ExprId cond_id = proc_state.AddExpr(*std::move(cond_or));
  auto body_or =
      LowerStatement(unit_facts, proc_state, scope_state, stack, w.stmt);
  if (!body_or) return std::unexpected(std::move(body_or.error()));
  const hir::StmtId body_id = proc_state.AddStmt(*std::move(body_or));
  std::vector<hir::SensitivityEntry> sensitivity;
  if (const auto* reads = unit_facts.SensitivityReads().Lookup(w.cond);
      reads != nullptr) {
    sensitivity =
        TranslateSensitivityReads(*reads, scope_state.UnitState(), stack);
  }
  return hir::Stmt{
      .label = std::nullopt,
      .data =
          hir::WaitStmt{
              .cond = cond_id,
              .body = body_id,
              .sensitivity_list = std::move(sensitivity)},
      .span = span};
}

auto LowerStatement(
    const UnitLoweringFacts& unit_facts, ProcessLoweringState& proc_state,
    ScopeLoweringState& scope_state, ScopeStack& stack,
    const slang::ast::Statement& stmt) -> diag::Result<hir::Stmt> {
  const auto& mapper = unit_facts.SourceMapper();
  const auto span = mapper.SpanOf(stmt.sourceRange);
  switch (stmt.kind) {
    case slang::ast::StatementKind::Empty:
      return LowerEmptyStmt(span);

    case slang::ast::StatementKind::EventTrigger:
      return LowerEventTriggerStmt(
          unit_facts, proc_state, scope_state, stack,
          stmt.as<slang::ast::EventTriggerStatement>(), span);

    case slang::ast::StatementKind::Wait:
      return LowerWaitStmt(
          unit_facts, proc_state, scope_state, stack,
          stmt.as<slang::ast::WaitStatement>(), span);

    case slang::ast::StatementKind::Timed:
      return LowerTimedStmt(
          unit_facts, proc_state, scope_state, stack,
          stmt.as<slang::ast::TimedStatement>(), span);

    case slang::ast::StatementKind::List:
      return LowerStatementListStmt(
          unit_facts, proc_state, scope_state, stack,
          stmt.as<slang::ast::StatementList>(), span);

    case slang::ast::StatementKind::Block:
      return LowerBlockStmt(
          unit_facts, proc_state, scope_state, stack,
          stmt.as<slang::ast::BlockStatement>(), span);

    case slang::ast::StatementKind::VariableDeclaration:
      return LowerVariableDeclStmt(
          unit_facts, proc_state, scope_state, stack,
          stmt.as<slang::ast::VariableDeclStatement>(), span);

    case slang::ast::StatementKind::ExpressionStatement:
      return LowerExpressionStmt(
          unit_facts, proc_state, scope_state, stack,
          stmt.as<slang::ast::ExpressionStatement>(), span);

    case slang::ast::StatementKind::ForLoop:
      return LowerForLoopStmt(
          unit_facts, proc_state, scope_state, stack,
          stmt.as<slang::ast::ForLoopStatement>(), span);

    case slang::ast::StatementKind::WhileLoop:
      return LowerWhileLoopStmt(
          unit_facts, proc_state, scope_state, stack,
          stmt.as<slang::ast::WhileLoopStatement>(), span);

    case slang::ast::StatementKind::RepeatLoop:
      return LowerRepeatLoopStmt(
          unit_facts, proc_state, scope_state, stack,
          stmt.as<slang::ast::RepeatLoopStatement>(), span);

    case slang::ast::StatementKind::DoWhileLoop:
      return LowerDoWhileLoopStmt(
          unit_facts, proc_state, scope_state, stack,
          stmt.as<slang::ast::DoWhileLoopStatement>(), span);

    case slang::ast::StatementKind::ForeverLoop:
      return LowerForeverLoopStmt(
          unit_facts, proc_state, scope_state, stack,
          stmt.as<slang::ast::ForeverLoopStatement>(), span);

    case slang::ast::StatementKind::Break:
      return LowerBreakStmt(span);

    case slang::ast::StatementKind::Continue:
      return LowerContinueStmt(span);

    case slang::ast::StatementKind::Case:
      return LowerCaseStmt(
          unit_facts, proc_state, scope_state, stack,
          stmt.as<slang::ast::CaseStatement>(), span);

    case slang::ast::StatementKind::Conditional:
      return LowerConditionalStmt(
          unit_facts, proc_state, scope_state, stack,
          stmt.as<slang::ast::ConditionalStatement>(), span);

    default:
      return diag::Unsupported(
          span, diag::DiagCode::kUnsupportedStatementForm,
          "this statement form is not supported yet",
          diag::UnsupportedCategory::kFeature);
  }
}

}  // namespace lyra::lowering::ast_to_hir
