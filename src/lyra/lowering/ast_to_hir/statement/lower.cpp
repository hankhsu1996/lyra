#include "lower.hpp"

#include <optional>
#include <utility>
#include <vector>

#include <slang/ast/Expression.h>
#include <slang/ast/Statement.h>
#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/statements/MiscStatements.h>

#include "../facts.hpp"
#include "../state.hpp"
#include "assignment.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/support/unsupported.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerStatement(
    const ProcessLoweringFacts& facts, ProcessLoweringState& state,
    const ModuleLoweringState& module_state, const slang::ast::Statement& stmt)
    -> hir::StmtId {
  switch (stmt.kind) {
    case slang::ast::StatementKind::List: {
      const auto& list = stmt.as<slang::ast::StatementList>();
      std::vector<hir::StmtId> kids;
      kids.reserve(list.list.size());
      for (const auto* child : list.list) {
        kids.push_back(LowerStatement(facts, state, module_state, *child));
      }
      return state.AppendStmt(
          hir::Stmt{
              .label = std::nullopt,
              .data = hir::BlockStmt{.statements = std::move(kids)}});
    }

    case slang::ast::StatementKind::Block: {
      const auto& block = stmt.as<slang::ast::BlockStatement>();
      return LowerStatement(facts, state, module_state, block.body);
    }

    case slang::ast::StatementKind::ExpressionStatement: {
      const auto& expr_stmt = stmt.as<slang::ast::ExpressionStatement>();
      if (expr_stmt.expr.kind != slang::ast::ExpressionKind::Assignment) {
        support::Unsupported(
            "LowerStatement: only assignment expression-statements supported");
      }
      return LowerAssignmentExpression(
          facts, state, module_state,
          expr_stmt.expr.as<slang::ast::AssignmentExpression>());
    }

    default:
      support::Unsupported("LowerStatement: unsupported statement kind");
  }
}

}  // namespace lyra::lowering::ast_to_hir
