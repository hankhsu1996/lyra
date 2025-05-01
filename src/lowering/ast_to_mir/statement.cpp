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
    -> std::vector<std::unique_ptr<mir::Statement>> {
  std::vector<std::unique_ptr<mir::Statement>> result;

  switch (statement.kind) {
    case StatementKind::List: {
      const auto& statement_list = statement.as<slang::ast::StatementList>();
      for (const auto* stmt : statement_list.list) {
        auto lowered = LowerStatement(*stmt);
        for (auto& stmt : lowered) {
          result.push_back(std::move(stmt));
        }
      }
      break;
    }

    case StatementKind::Block: {
      const auto& block_statement = statement.as<slang::ast::BlockStatement>();
      auto lowered = LowerStatement(block_statement.body);
      for (auto& stmt : lowered) {
        result.push_back(std::move(stmt));
      }
      break;
    }

    case StatementKind::ExpressionStatement: {
      const auto& expression_statement =
          statement.as<slang::ast::ExpressionStatement>();
      const auto& expression = expression_statement.expr;

      auto lowered_expression = LowerExpression(expression);
      result.push_back(std::make_unique<mir::ExpressionStatement>(
          std::move(lowered_expression)));
      break;
    }

    case StatementKind::Timed: {
      const auto& timed_statement = statement.as<slang::ast::TimedStatement>();
      const auto& timing_control = timed_statement.timing;

      if (timing_control.kind == TimingControlKind::Delay) {
        const auto& delay_control =
            timing_control.as<slang::ast::DelayControl>();
        const auto& expr = delay_control.expr;

        if (expr.kind == slang::ast::ExpressionKind::IntegerLiteral) {
          const auto& int_literal = expr.as<slang::ast::IntegerLiteral>();
          auto delay_amount_opt = int_literal.getValue().as<int64_t>();

          if (delay_amount_opt) {
            result.push_back(std::make_unique<mir::DelayStatement>(
                delay_amount_opt.value()));
            auto inner = LowerStatement(timed_statement.stmt);
            for (auto& stmt : inner) {
              result.push_back(std::move(stmt));
            }
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
      break;  // no-op
    }

    default:
      throw std::runtime_error(fmt::format(
          "Unsupported statement kind {} in LowerStatement",
          slang::ast::toString(statement.kind)));
  }

  return result;
}

}  // namespace lyra::lowering
