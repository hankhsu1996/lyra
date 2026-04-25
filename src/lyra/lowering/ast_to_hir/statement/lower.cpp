#include "lower.hpp"

#include <expected>
#include <optional>
#include <utility>
#include <vector>

#include <slang/ast/Expression.h>
#include <slang/ast/Statement.h>
#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/statements/MiscStatements.h>

#include "../expression/lower.hpp"
#include "../facts.hpp"
#include "../state.hpp"
#include "assignment.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/stmt.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerStatement(
    const UnitLoweringFacts& unit_facts, ProcessLoweringState& proc_state,
    ScopeLoweringState& scope_state, ScopeStack& stack,
    const slang::ast::Statement& stmt) -> diag::Result<hir::Stmt> {
  const auto& mapper = unit_facts.SourceMapper();
  switch (stmt.kind) {
    case slang::ast::StatementKind::List: {
      const auto& list = stmt.as<slang::ast::StatementList>();
      std::vector<hir::StmtId> kids;
      kids.reserve(list.list.size());
      for (const auto* child : list.list) {
        auto child_stmt =
            LowerStatement(unit_facts, proc_state, scope_state, stack, *child);
        if (!child_stmt) return std::unexpected(std::move(child_stmt.error()));
        kids.push_back(proc_state.AppendStmt(*std::move(child_stmt)));
      }
      return hir::Stmt{
          .label = std::nullopt,
          .data = hir::BlockStmt{.statements = std::move(kids)}};
    }

    case slang::ast::StatementKind::Block: {
      const auto& block = stmt.as<slang::ast::BlockStatement>();
      return LowerStatement(
          unit_facts, proc_state, scope_state, stack, block.body);
    }

    case slang::ast::StatementKind::ExpressionStatement: {
      const auto& expr_stmt = stmt.as<slang::ast::ExpressionStatement>();
      if (expr_stmt.expr.kind != slang::ast::ExpressionKind::Assignment) {
        return diag::Unsupported(
            mapper.SpanOf(expr_stmt.sourceRange),
            "only assignment expression-statements are supported",
            diag::UnsupportedCategory::kFeature);
      }
      const auto& assign =
          expr_stmt.expr.as<slang::ast::AssignmentExpression>();

      auto rhs = LowerExpressionData(
          unit_facts, scope_state.UnitState(), stack, assign.right());
      if (!rhs) return std::unexpected(std::move(rhs.error()));
      const hir::ExprId rhs_id = proc_state.AppendExpr(*std::move(rhs));

      auto stmt_data = LowerAssignmentStatementData(
          unit_facts, scope_state.UnitState(), stack, assign, rhs_id);
      if (!stmt_data) return std::unexpected(std::move(stmt_data.error()));

      return hir::Stmt{.label = std::nullopt, .data = *std::move(stmt_data)};
    }

    default:
      return diag::Unsupported(
          mapper.SpanOf(stmt.sourceRange),
          "this statement form is not supported yet",
          diag::UnsupportedCategory::kFeature);
  }
}

}  // namespace lyra::lowering::ast_to_hir
