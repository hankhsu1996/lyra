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

#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/kind.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/ast_to_hir/expression/lower.hpp"
#include "lyra/lowering/ast_to_hir/facts.hpp"
#include "lyra/lowering/ast_to_hir/state.hpp"
#include "lyra/lowering/ast_to_hir/type.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

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
    case slang::ast::TimingControlKind::SignalEvent:
    case slang::ast::TimingControlKind::EventList:
    case slang::ast::TimingControlKind::ImplicitEvent:
    case slang::ast::TimingControlKind::RepeatedEvent:
      return hir::TimingControl{hir::EventControl{}};
    default:
      return diag::Unsupported(
          span, diag::DiagCode::kUnsupportedTimingControlKind,
          "this timing control kind is not yet supported",
          diag::UnsupportedCategory::kFeature);
  }
}

}  // namespace

auto LowerStatement(
    const UnitLoweringFacts& unit_facts, ProcessLoweringState& proc_state,
    ScopeLoweringState& scope_state, ScopeStack& stack,
    const slang::ast::Statement& stmt) -> diag::Result<hir::Stmt> {
  const auto& mapper = unit_facts.SourceMapper();
  const auto span = mapper.SpanOf(stmt.sourceRange);
  switch (stmt.kind) {
    case slang::ast::StatementKind::Empty: {
      return hir::Stmt{
          .label = std::nullopt, .data = hir::EmptyStmt{}, .span = span};
    }

    case slang::ast::StatementKind::Timed: {
      const auto& ts = stmt.as<slang::ast::TimedStatement>();
      auto timing = LowerTimingControl(
          unit_facts, scope_state.UnitState(), proc_state, stack, ts.timing,
          span);
      if (!timing) return std::unexpected(std::move(timing.error()));
      auto inner_stmt =
          LowerStatement(unit_facts, proc_state, scope_state, stack, ts.stmt);
      if (!inner_stmt) return std::unexpected(std::move(inner_stmt.error()));
      const hir::StmtId inner_id = proc_state.AddStmt(*std::move(inner_stmt));
      return hir::Stmt{
          .label = std::nullopt,
          .data =
              hir::TimedStmt{.timing = *std::move(timing), .stmt = inner_id},
          .span = span};
    }

    case slang::ast::StatementKind::List: {
      const auto& list = stmt.as<slang::ast::StatementList>();
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

    case slang::ast::StatementKind::Block: {
      const auto& block = stmt.as<slang::ast::BlockStatement>();
      std::vector<hir::StmtId> kids;
      if (block.body.kind == slang::ast::StatementKind::List) {
        const auto& list = block.body.as<slang::ast::StatementList>();
        kids.reserve(list.list.size());
        for (const auto* child : list.list) {
          auto child_stmt = LowerStatement(
              unit_facts, proc_state, scope_state, stack, *child);
          if (!child_stmt)
            return std::unexpected(std::move(child_stmt.error()));
          kids.push_back(proc_state.AddStmt(*std::move(child_stmt)));
        }
      } else {
        auto child_stmt = LowerStatement(
            unit_facts, proc_state, scope_state, stack, block.body);
        if (!child_stmt) return std::unexpected(std::move(child_stmt.error()));
        kids.push_back(proc_state.AddStmt(*std::move(child_stmt)));
      }
      return hir::Stmt{
          .label = std::nullopt,
          .data = hir::BlockStmt{.statements = std::move(kids)},
          .span = span};
    }

    case slang::ast::StatementKind::VariableDeclaration: {
      const auto& vd = stmt.as<slang::ast::VariableDeclStatement>();
      const auto& sym = vd.symbol;
      auto type_data =
          LowerTypeData(sym.getType(), mapper.PointSpanOf(sym.location));
      if (!type_data) return std::unexpected(std::move(type_data.error()));
      const auto type_id =
          scope_state.UnitState().AddType(*std::move(type_data));
      const auto local_id = proc_state.AddProceduralVar(sym, type_id);
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

    case slang::ast::StatementKind::ExpressionStatement: {
      const auto& es = stmt.as<slang::ast::ExpressionStatement>();
      auto expr = LowerProcExpr(
          unit_facts, scope_state.UnitState(), proc_state, stack, es.expr);
      if (!expr) return std::unexpected(std::move(expr.error()));
      const hir::ExprId id = proc_state.AddExpr(*std::move(expr));
      return hir::Stmt{
          .label = std::nullopt,
          .data = hir::ExprStmt{.expr = id},
          .span = span};
    }

    case slang::ast::StatementKind::ForLoop: {
      const auto& fs = stmt.as<slang::ast::ForLoopStatement>();
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
            unit_facts, scope_state.UnitState(), proc_state, stack,
            *fs.stopExpr);
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

    case slang::ast::StatementKind::WhileLoop: {
      const auto& ws = stmt.as<slang::ast::WhileLoopStatement>();
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

    case slang::ast::StatementKind::Case: {
      const auto& cs = stmt.as<slang::ast::CaseStatement>();
      if (cs.condition != slang::ast::CaseStatementCondition::Normal) {
        return diag::Unsupported(
            span, diag::DiagCode::kUnsupportedStatementForm,
            "casez/casex/case-inside are not yet supported",
            diag::UnsupportedCategory::kFeature);
      }
      if (cs.check != slang::ast::UniquePriorityCheck::None) {
        return diag::Unsupported(
            span, diag::DiagCode::kUnsupportedStatementForm,
            "unique/priority qualifiers on case are not yet supported",
            diag::UnsupportedCategory::kFeature);
      }
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
              unit_facts, scope_state.UnitState(), proc_state, stack,
              *label_expr);
          if (!label_or) return std::unexpected(std::move(label_or.error()));
          label_ids.push_back(proc_state.AddExpr(*std::move(label_or)));
        }
        auto item_stmt = LowerStatement(
            unit_facts, proc_state, scope_state, stack, *item.stmt);
        if (!item_stmt) return std::unexpected(std::move(item_stmt.error()));
        const hir::StmtId item_id = proc_state.AddStmt(*std::move(item_stmt));
        items.push_back(
            hir::CaseItem{.labels = std::move(label_ids), .stmt = item_id});
      }
      std::optional<hir::StmtId> default_id;
      if (cs.defaultCase != nullptr) {
        auto default_stmt = LowerStatement(
            unit_facts, proc_state, scope_state, stack, *cs.defaultCase);
        if (!default_stmt) {
          return std::unexpected(std::move(default_stmt.error()));
        }
        default_id = proc_state.AddStmt(*std::move(default_stmt));
      }
      return hir::Stmt{
          .label = std::nullopt,
          .data =
              hir::CaseStmt{
                  .condition = cond_id,
                  .items = std::move(items),
                  .default_stmt = default_id},
          .span = span};
    }

    case slang::ast::StatementKind::Conditional: {
      const auto& cs = stmt.as<slang::ast::ConditionalStatement>();
      if (cs.check != slang::ast::UniquePriorityCheck::None) {
        return diag::Unsupported(
            span, diag::DiagCode::kUnsupportedStatementForm,
            "unique/priority qualifiers on if are not yet supported",
            diag::UnsupportedCategory::kFeature);
      }
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
        auto else_stmt = LowerStatement(
            unit_facts, proc_state, scope_state, stack, *cs.ifFalse);
        if (!else_stmt) return std::unexpected(std::move(else_stmt.error()));
        else_id = proc_state.AddStmt(*std::move(else_stmt));
      }
      return hir::Stmt{
          .label = std::nullopt,
          .data =
              hir::IfStmt{
                  .condition = cond_id,
                  .then_stmt = then_id,
                  .else_stmt = else_id},
          .span = span};
    }

    default:
      return diag::Unsupported(
          span, diag::DiagCode::kUnsupportedStatementForm,
          "this statement form is not supported yet",
          diag::UnsupportedCategory::kFeature);
  }
}

}  // namespace lyra::lowering::ast_to_hir
