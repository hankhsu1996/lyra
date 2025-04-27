#include "lowering/ast_to_mir/statement.hpp"

#include <stdexcept>

#include <fmt/format.h>
#include <slang/ast/Expression.h>
#include <slang/ast/Statement.h>
#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/statements/MiscStatements.h>
#include <spdlog/spdlog.h>

#include "lowering/ast_to_mir/expression.hpp"
#include "mir/statement.hpp"

namespace lyra::lowering {

auto LowerStatement(const slang::ast::Statement& statement)
    -> std::vector<std::shared_ptr<mir::Statement>> {
  using StatementKind = slang::ast::StatementKind;
  std::vector<std::shared_ptr<mir::Statement>> result;

  switch (statement.kind) {
    case StatementKind::List: {
      const auto& statement_list = statement.as<slang::ast::StatementList>();
      for (const auto* statement : statement_list.list) {
        auto lowered_statements = LowerStatement(*statement);
        result.insert(
            result.end(), lowered_statements.begin(), lowered_statements.end());
      }
      break;
    }
    case StatementKind::Block: {
      const auto& block_statement = statement.as<slang::ast::BlockStatement>();
      auto inner_statements = LowerStatement(block_statement.body);
      result.insert(
          result.end(), inner_statements.begin(), inner_statements.end());
      break;
    }
    case StatementKind::ExpressionStatement: {
      const auto& expression_statement =
          statement.as<slang::ast::ExpressionStatement>();
      const auto& expression = expression_statement.expr;

      switch (expression.kind) {
        case slang::ast::ExpressionKind::Assignment: {
          const auto& assignment_expression =
              expression.as<slang::ast::AssignmentExpression>();

          const auto& left_expression = assignment_expression.left();

          if (left_expression.kind != slang::ast::ExpressionKind::NamedValue) {
            throw std::runtime_error(fmt::format(
                "Unsupported assignment target expression kind {} in "
                "LowerStatement",
                slang::ast::toString(left_expression.kind)));
          }

          auto target_name =
              left_expression.as<slang::ast::NamedValueExpression>()
                  .symbol.name;

          auto mir_statement = std::make_shared<mir::Statement>();
          mir_statement->kind = mir::Statement::Kind::kAssign;
          mir_statement->target = std::string(target_name);
          mir_statement->value = LowerExpression(assignment_expression.right());

          result.push_back(std::move(mir_statement));
          break;
        }
        default:
          throw std::runtime_error(fmt::format(
              "Unsupported expression kind {} in LowerStatement",
              slang::ast::toString(expression.kind)));
      }
      break;
    }
    default:
      throw std::runtime_error(fmt::format(
          "Unsupported statement kind {} in LowerStatement",
          slang::ast::toString(statement.kind)));
  }

  return result;
}

}  // namespace lyra::lowering
