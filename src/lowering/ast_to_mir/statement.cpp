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

auto LowerTimingControl(const slang::ast::TimingControl& timing_control)
    -> common::TimingControl {
  switch (timing_control.kind) {
    case TimingControlKind::Delay: {
      const auto& delay_control = timing_control.as<slang::ast::DelayControl>();
      const auto& expr = delay_control.expr;

      if (expr.kind == slang::ast::ExpressionKind::IntegerLiteral) {
        const auto& int_literal = expr.as<slang::ast::IntegerLiteral>();
        auto delay_amount_opt = int_literal.getValue().as<int64_t>();
        if (!delay_amount_opt) {
          throw std::runtime_error(
              "Only constant integer delay is supported in timing control "
              "lowering");
        }
        return common::TimingControl::Delay(delay_amount_opt.value());
      }

      throw std::runtime_error(fmt::format(
          "Unsupported timing control kind {} in LowerTimingControl",
          slang::ast::toString(timing_control.kind)));
    }

    default:
      throw std::runtime_error(fmt::format(
          "Unsupported timing control kind {} in LowerTimingControl",
          slang::ast::toString(timing_control.kind)));
  }
}

auto LowerStatement(
    const slang::ast::Statement& statement,
    std::optional<common::TimingControl> timing)
    -> std::vector<std::shared_ptr<mir::Statement>> {
  std::vector<std::shared_ptr<mir::Statement>> result;

  switch (statement.kind) {
    case StatementKind::List: {
      const auto& statement_list = statement.as<slang::ast::StatementList>();
      for (const auto* stmt : statement_list.list) {
        auto lowered = LowerStatement(*stmt, timing);
        result.insert(result.end(), lowered.begin(), lowered.end());
      }
      break;
    }

    case StatementKind::Block: {
      const auto& block_statement = statement.as<slang::ast::BlockStatement>();
      auto lowered = LowerStatement(block_statement.body, timing);
      result.insert(result.end(), lowered.begin(), lowered.end());
      break;
    }

    case StatementKind::ExpressionStatement: {
      const auto& expr_stmt = statement.as<slang::ast::ExpressionStatement>();
      const auto& expr = expr_stmt.expr;

      switch (expr.kind) {
        case ExpressionKind::Assignment: {
          const auto& assignment = expr.as<slang::ast::AssignmentExpression>();
          const auto& left = assignment.left();

          if (left.kind != ExpressionKind::NamedValue) {
            throw std::runtime_error(fmt::format(
                "Unsupported assignment target kind {} in LowerStatement",
                slang::ast::toString(left.kind)));
          }

          auto target_name =
              left.as<slang::ast::NamedValueExpression>().symbol.name;

          auto assign_stmt = std::make_shared<mir::AssignStatement>(
              std::string(target_name), LowerExpression(assignment.right()),
              timing);

          result.push_back(std::move(assign_stmt));
          break;
        }

        default:
          throw std::runtime_error(fmt::format(
              "Unsupported expression kind {} in LowerStatement",
              slang::ast::toString(expr.kind)));
      }
      break;
    }

    case StatementKind::Timed: {
      const auto& timed_stmt = statement.as<slang::ast::TimedStatement>();
      auto lowered_timing = LowerTimingControl(timed_stmt.timing);
      auto inner = LowerStatement(timed_stmt.stmt, lowered_timing);
      result.insert(result.end(), inner.begin(), inner.end());
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
