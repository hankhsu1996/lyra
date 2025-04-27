#include "lowering/ast_to_mir/collect_sensitivity.hpp"

#include <unordered_set>

#include <fmt/format.h>
#include <slang/ast/ASTVisitor.h>
#include <slang/ast/Expression.h>
#include <slang/ast/Statement.h>
#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/expressions/LiteralExpressions.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/expressions/OperatorExpressions.h>
#include <slang/ast/statements/MiscStatements.h>
#include <spdlog/spdlog.h>

namespace lyra::lowering {

auto CollectSensitivityList(const slang::ast::Statement& statement)
    -> std::vector<std::string> {
  spdlog::info("Collecting sensitivity list");
  std::unordered_set<std::string> signals;

  auto visitor = slang::ast::makeVisitor(
      [&](const slang::ast::NamedValueExpression& expr) {
        if (!expr.symbol.name.empty()) {
          signals.insert(std::string(expr.symbol.name));
        } else {
          spdlog::warn("Empty signal name found in sensitivity list");
        }
      });

  auto visit_expression = [&](const slang::ast::Expression& expr) {
    visitor.visit(expr);
  };

  auto visit_statement = [&](const slang::ast::Statement& statement,
                             auto&& self) -> void {
    using Kind = slang::ast::StatementKind;

    switch (statement.kind) {
      case Kind::List: {
        const auto& statement_list = statement.as<slang::ast::StatementList>();
        for (const auto* statement : statement_list.list) {
          self(*statement, self);
        }
        break;
      }
      case Kind::Block: {
        const auto& block_statement =
            statement.as<slang::ast::BlockStatement>();
        self(block_statement.body, self);
        break;
      }
      case Kind::ExpressionStatement: {
        const auto& expression_statement =
            statement.as<slang::ast::ExpressionStatement>();
        visit_expression(expression_statement.expr);
        break;
      }
      default:
        throw std::runtime_error(fmt::format(
            "Unsupported statement kind {} in CollectSensitivityList",
            slang::ast::toString(statement.kind)));
    }
  };

  visit_statement(statement, visit_statement);

  return {signals.begin(), signals.end()};
}

}  // namespace lyra::lowering
