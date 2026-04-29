#include "lyra/lowering/ast_to_hir/statement/lower.hpp"

#include <expected>
#include <optional>
#include <utility>
#include <vector>

#include <slang/ast/Expression.h>
#include <slang/ast/Statement.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/TimingControl.h>
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
  if (tc.kind == slang::ast::TimingControlKind::Delay) {
    const auto& delay = tc.as<slang::ast::DelayControl>();
    auto duration =
        LowerProcExpr(unit_facts, unit_state, proc_state, stack, delay.expr);
    if (!duration) return std::unexpected(std::move(duration.error()));
    return hir::TimingControl{hir::DelayControl{
        .duration = proc_state.AddExpr(*std::move(duration))}};
  }
  return diag::Unsupported(
      span, diag::DiagCode::kUnsupportedTimingControlKind,
      "this timing control kind is not yet supported",
      diag::UnsupportedCategory::kFeature);
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
      auto body_stmt =
          LowerStatement(unit_facts, proc_state, scope_state, stack, ts.stmt);
      if (!body_stmt) return std::unexpected(std::move(body_stmt.error()));
      const hir::StmtId body_id = proc_state.AddStmt(*std::move(body_stmt));
      return hir::Stmt{
          .label = std::nullopt,
          .data = hir::TimedStmt{.timing = *std::move(timing), .body = body_id},
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
      const auto local_id = proc_state.AddLocalVar(sym, type_id);
      return hir::Stmt{
          .label = std::nullopt,
          .data = hir::VarDeclStmt{.local_var = local_id},
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

    default:
      return diag::Unsupported(
          span, diag::DiagCode::kUnsupportedStatementForm,
          "this statement form is not supported yet",
          diag::UnsupportedCategory::kFeature);
  }
}

}  // namespace lyra::lowering::ast_to_hir
