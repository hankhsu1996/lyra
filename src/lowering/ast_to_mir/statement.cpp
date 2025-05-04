#include "lowering/ast_to_mir/statement.hpp"

#include <stdexcept>

#include <fmt/format.h>
#include <slang/ast/Expression.h>
#include <slang/ast/Statement.h>
#include <slang/ast/TimingControl.h>
#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/expressions/LiteralExpressions.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/statements/ConditionalStatements.h>
#include <slang/ast/statements/LoopStatements.h>
#include <slang/ast/statements/MiscStatements.h>

#include "lowering/ast_to_mir/expression.hpp"
#include "mir/statement.hpp"

namespace lyra::lowering {

using StatementKind = slang::ast::StatementKind;
using ExpressionKind = slang::ast::ExpressionKind;
using TimingControlKind = slang::ast::TimingControlKind;

auto LowerStatement(const slang::ast::Statement& statement)
    -> std::unique_ptr<mir::Statement> {
  switch (statement.kind) {
    case StatementKind::List: {
      const auto& statement_list = statement.as<slang::ast::StatementList>();
      auto block = std::make_unique<mir::BlockStatement>();
      for (const auto* statement : statement_list.list) {
        block->statements.push_back(LowerStatement(*statement));
      }
      return block;
    }

    case StatementKind::Block: {
      const auto& block_statement = statement.as<slang::ast::BlockStatement>();
      return LowerStatement(block_statement.body);
    }

    case StatementKind::ExpressionStatement: {
      const auto& expression_statement =
          statement.as<slang::ast::ExpressionStatement>();
      auto lowered_expression = LowerExpression(expression_statement.expr);
      return std::make_unique<mir::ExpressionStatement>(
          std::move(lowered_expression));
    }

    case StatementKind::Timed: {
      const auto& timed_statement = statement.as<slang::ast::TimedStatement>();
      const auto& timing_control = timed_statement.timing;

      if (timing_control.kind != TimingControlKind::Delay) {
        throw std::runtime_error(fmt::format(
            "Unsupported timing control kind {} in AST to MIR LowerStatement",
            slang::ast::toString(timing_control.kind)));
      }

      const auto& delay_control = timing_control.as<slang::ast::DelayControl>();
      const auto& expression = delay_control.expr;

      if (expression.kind != ExpressionKind::IntegerLiteral) {
        throw std::runtime_error(fmt::format(
            "Unsupported delay expression kind {}",
            slang::ast::toString(expression.kind)));
      }

      const auto& int_literal = expression.as<slang::ast::IntegerLiteral>();
      auto delay_amount_opt = int_literal.getValue().as<uint64_t>();
      if (!delay_amount_opt) {
        throw std::runtime_error(
            "Only constant integer delay is supported in timing control "
            "lowering");
      }

      auto delay_statement =
          std::make_unique<mir::DelayStatement>(delay_amount_opt.value());
      auto inner_statement = LowerStatement(timed_statement.stmt);

      auto block = std::make_unique<mir::BlockStatement>();
      block->statements.push_back(std::move(delay_statement));
      block->statements.push_back(std::move(inner_statement));
      return block;
    }

    case StatementKind::Conditional: {
      const auto& conditional =
          statement.as<slang::ast::ConditionalStatement>();

      if (conditional.conditions.size() != 1) {
        throw std::runtime_error(
            "Multiple conditions in conditional statement are not supported");
      }

      auto condition = LowerExpression(*conditional.conditions[0].expr);
      if (!condition) {
        throw std::runtime_error("Failed to lower condition expression");
      }

      auto then_branch = LowerStatement(conditional.ifTrue);

      std::unique_ptr<mir::Statement> else_branch;
      if (conditional.ifFalse != nullptr) {
        else_branch = LowerStatement(*conditional.ifFalse);
      }

      return std::make_unique<mir::ConditionalStatement>(
          std::move(condition), std::move(then_branch), std::move(else_branch));
    }

    case StatementKind::WhileLoop: {
      const auto& while_loop = statement.as<slang::ast::WhileLoopStatement>();
      auto condition = LowerExpression(while_loop.cond);
      auto body = LowerStatement(while_loop.body);
      return std::make_unique<mir::WhileStatement>(
          std::move(condition), std::move(body));
    }

    case StatementKind::DoWhileLoop: {
      const auto& do_while_loop =
          statement.as<slang::ast::DoWhileLoopStatement>();
      auto condition = LowerExpression(do_while_loop.cond);
      auto body = LowerStatement(do_while_loop.body);
      return std::make_unique<mir::DoWhileStatement>(
          std::move(condition), std::move(body));
    }

    case StatementKind::Empty: {
      return std::make_unique<mir::BlockStatement>();  // No-op
    }

    default:
      throw std::runtime_error(fmt::format(
          "Unsupported statement kind {} in AST to MIR LowerStatement",
          slang::ast::toString(statement.kind)));
  }
}

}  // namespace lyra::lowering
