#include "lowering/ast_to_mir/statement.hpp"

#include <stdexcept>
#include <vector>

#include <fmt/format.h>
#include <slang/ast/Expression.h>
#include <slang/ast/Statement.h>
#include <slang/ast/TimingControl.h>
#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/expressions/LiteralExpressions.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/statements/MiscStatements.h>

#include "lowering/ast_to_mir/expression.hpp"
#include "mir/statement.hpp"

namespace lyra::lowering {

using StatementKind = slang::ast::StatementKind;
using ExpressionKind = slang::ast::ExpressionKind;
using TimingControlKind = slang::ast::TimingControlKind;

auto LowerStatement(const slang::ast::Statement& statement)
    -> std::vector<std::shared_ptr<mir::Statement>> {
  std::vector<std::shared_ptr<mir::Statement>> result;

  switch (statement.kind) {
    case StatementKind::List: {
      const auto& statement_list = statement.as<slang::ast::StatementList>();
      for (const auto* stmt : statement_list.list) {
        auto lowered = LowerStatement(*stmt);
        result.insert(result.end(), lowered.begin(), lowered.end());
      }
      break;
    }

    case StatementKind::Block: {
      const auto& block_statement = statement.as<slang::ast::BlockStatement>();
      auto lowered = LowerStatement(block_statement.body);
      result.insert(result.end(), lowered.begin(), lowered.end());
      break;
    }

    case StatementKind::ExpressionStatement: {
      const auto& expression_statement =
          statement.as<slang::ast::ExpressionStatement>();
      const auto& expression = expression_statement.expr;

      // Lower the expression
      auto lowered_expression = LowerExpression(expression);

      // Create an expression statement with the lowered expression
      auto lowered_expression_statement =
          std::make_shared<mir::ExpressionStatement>(lowered_expression);
      result.push_back(std::move(lowered_expression_statement));
      break;
    }

    case StatementKind::Timed: {
      const auto& timed_statement = statement.as<slang::ast::TimedStatement>();
      const auto& timing_control = timed_statement.timing;

      // Extract delay amount directly
      if (timing_control.kind == TimingControlKind::Delay) {
        const auto& delay_control =
            timing_control.as<slang::ast::DelayControl>();
        const auto& expr = delay_control.expr;

        if (expr.kind == slang::ast::ExpressionKind::IntegerLiteral) {
          const auto& int_literal = expr.as<slang::ast::IntegerLiteral>();
          auto delay_amount_opt = int_literal.getValue().as<int64_t>();

          if (delay_amount_opt) {
            // Create a delay statement with the extracted amount
            auto delay_statement =
                std::make_shared<mir::DelayStatement>(delay_amount_opt.value());
            result.push_back(delay_statement);

            // Process the inner statement
            auto inner = LowerStatement(timed_statement.stmt);
            result.insert(result.end(), inner.begin(), inner.end());
          } else {
            throw std::runtime_error(
                "Only constant integer delay is supported in timing control "
                "lowering");
          }
        } else {
          throw std::runtime_error(fmt::format(
              "Unsupported expression kind in delay control: {}",
              slang::ast::toString(expr.kind)));
        }
      } else {
        throw std::runtime_error(fmt::format(
            "Unsupported timing control kind: {}",
            slang::ast::toString(timing_control.kind)));
      }
      break;
    }

    case StatementKind::Empty: {
      // Empty statement - don't generate any MIR
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
