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
#include <slang/ast/statements/ConditionalStatements.h>
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
              "Unsupported expression kind {} in AST to MIR LowerStatement",
              slang::ast::toString(expr.kind)));
        }
      } else {
        throw std::runtime_error(fmt::format(
            "Unsupported timing control kind {} in AST to MIR LowerStatement",
            slang::ast::toString(timing_control.kind)));
      }
      break;
    }

    case StatementKind::Empty: {
      break;  // no-op
    }

    case StatementKind::Conditional: {
      const auto& conditional =
          statement.as<slang::ast::ConditionalStatement>();

      // if more than one, that is tagged union, we dont support yet.
      if (conditional.conditions.size() != 1) {
        throw std::runtime_error(
            "Multiple conditions in conditional statement are not supported");
      }

      // Lower the condition expression
      auto condition = LowerExpression(*conditional.conditions[0].expr);
      if (!condition) {
        throw std::runtime_error("Failed to lower condition expression");
      }

      // Lower the then branch statement(s)
      auto then_statements = LowerStatement(conditional.ifTrue);
      if (then_statements.empty()) {
        throw std::runtime_error("Failed to lower 'then' branch statements");
      }

      // Convert multiple statements into a block if needed
      std::unique_ptr<mir::Statement> then_branch;
      if (then_statements.size() == 1) {
        then_branch = std::move(then_statements[0]);
      } else {
        auto block = std::make_unique<mir::BlockStatement>();
        for (auto& stmt : then_statements) {
          block->statements.push_back(std::move(stmt));
        }
        then_branch = std::move(block);
      }

      // Handle the else branch if it exists
      std::unique_ptr<mir::Statement> else_branch;
      if (conditional.ifFalse != nullptr) {
        auto else_statements = LowerStatement(*conditional.ifFalse);
        if (else_statements.size() == 1) {
          else_branch = std::move(else_statements[0]);
        } else {
          auto block = std::make_unique<mir::BlockStatement>();
          for (auto& stmt : else_statements) {
            block->statements.push_back(std::move(stmt));
          }
          else_branch = std::move(block);
        }
      }

      // Create the if statement
      auto if_statement = std::make_unique<mir::ConditionalStatement>(
          std::move(condition), std::move(then_branch), std::move(else_branch));

      result.push_back(std::move(if_statement));
      break;
    }

    default:
      throw std::runtime_error(fmt::format(
          "Unsupported statement kind {} in AST to MIR LowerStatement",
          slang::ast::toString(statement.kind)));
  }

  return result;
}

}  // namespace lyra::lowering
