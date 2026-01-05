#include "lyra/lowering/ast_to_mir/statement.hpp"

#include <fmt/format.h>
#include <slang/ast/Expression.h>
#include <slang/ast/SemanticFacts.h>
#include <slang/ast/Statement.h>
#include <slang/ast/TimingControl.h>
#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/expressions/LiteralExpressions.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/statements/ConditionalStatements.h>
#include <slang/ast/statements/LoopStatements.h>
#include <slang/ast/statements/MiscStatements.h>
#include <slang/ast/symbols/VariableSymbols.h>

#include "lyra/common/diagnostic.hpp"
#include "lyra/lowering/ast_to_mir/expression.hpp"
#include "lyra/lowering/ast_to_mir/type.hpp"
#include "lyra/mir/statement.hpp"

namespace lyra::lowering::ast_to_mir {

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

    case StatementKind::VariableDeclaration: {
      const auto& declaration =
          statement.as<slang::ast::VariableDeclStatement>();
      return LowerVariableDeclaration(declaration.symbol);
    }

    case StatementKind::ExpressionStatement: {
      const auto& expression_statement =
          statement.as<slang::ast::ExpressionStatement>();
      return LowerExpressionStatement(expression_statement.expr);
    }

    case StatementKind::Timed: {
      const auto& timed_statement = statement.as<slang::ast::TimedStatement>();
      const auto& timing_control = timed_statement.timing;

      switch (timing_control.kind) {
        case slang::ast::TimingControlKind::Invalid: {
          throw DiagnosticException(
              Diagnostic::Error(
                  statement.sourceRange,
                  fmt::format(
                      "unsupported timing control kind '{}'",
                      slang::ast::toString(timing_control.kind))));
        }
        case slang::ast::TimingControlKind::Delay: {
          const auto& delay_control =
              timing_control.as<slang::ast::DelayControl>();
          const auto& expression = delay_control.expr;

          if (expression.kind != ExpressionKind::IntegerLiteral) {
            throw DiagnosticException(
                Diagnostic::Error(
                    expression.sourceRange,
                    fmt::format(
                        "unsupported delay expression kind '{}'",
                        slang::ast::toString(expression.kind))));
          }

          const auto& int_literal = expression.as<slang::ast::IntegerLiteral>();
          auto delay_amount_opt = int_literal.getValue().as<uint64_t>();
          if (!delay_amount_opt) {
            throw DiagnosticException(
                Diagnostic::Error(
                    expression.sourceRange,
                    "only constant integer delay is supported"));
          }

          auto delay_statement =
              std::make_unique<mir::DelayStatement>(delay_amount_opt.value());
          auto inner_statement = LowerStatement(timed_statement.stmt);

          auto block = std::make_unique<mir::BlockStatement>();
          block->statements.push_back(std::move(delay_statement));
          block->statements.push_back(std::move(inner_statement));
          return block;
        }

        case slang::ast::TimingControlKind::SignalEvent: {
          const auto& signal_event_control =
              timing_control.as<slang::ast::SignalEventControl>();
          const auto& expr = signal_event_control.expr;

          if (expr.kind != ExpressionKind::NamedValue) {
            throw DiagnosticException(
                Diagnostic::Error(
                    expr.sourceRange,
                    "only simple identifier supported in signal event "
                    "expression"));
          }

          const auto& named_expr = expr.as<slang::ast::NamedValueExpression>();
          const auto& variable = named_expr.symbol;

          common::EdgeKind edge_kind{};
          switch (signal_event_control.edge) {
            case slang::ast::EdgeKind::None:
              edge_kind = common::EdgeKind::kAnyChange;
              break;
            case slang::ast::EdgeKind::PosEdge:
              edge_kind = common::EdgeKind::kPosedge;
              break;
            case slang::ast::EdgeKind::NegEdge:
              edge_kind = common::EdgeKind::kNegedge;
              break;
            case slang::ast::EdgeKind::BothEdges:
              edge_kind = common::EdgeKind::kBothEdge;
              break;
          }

          auto trigger =
              common::Trigger{.edge_kind = edge_kind, .variable = &variable};
          std::vector<common::Trigger> triggers = {trigger};

          auto wait_statement =
              std::make_unique<mir::WaitEventStatement>(std::move(triggers));
          auto inner_statement = LowerStatement(timed_statement.stmt);

          auto block = std::make_unique<mir::BlockStatement>();
          block->statements.push_back(std::move(wait_statement));
          block->statements.push_back(std::move(inner_statement));
          return block;
        }
        case slang::ast::TimingControlKind::EventList:
        case slang::ast::TimingControlKind::ImplicitEvent:
        case slang::ast::TimingControlKind::RepeatedEvent:
        case slang::ast::TimingControlKind::Delay3:
        case slang::ast::TimingControlKind::OneStepDelay:
        case slang::ast::TimingControlKind::CycleDelay:
        case slang::ast::TimingControlKind::BlockEventList:
          throw DiagnosticException(
              Diagnostic::Error(
                  statement.sourceRange,
                  fmt::format(
                      "unsupported timing control kind '{}'",
                      slang::ast::toString(timing_control.kind))));
      }
    }

    case StatementKind::Conditional: {
      const auto& conditional =
          statement.as<slang::ast::ConditionalStatement>();

      if (conditional.conditions.size() != 1) {
        throw DiagnosticException(
            Diagnostic::Error(
                statement.sourceRange,
                "multiple conditions in conditional statement are not "
                "supported"));
      }

      auto condition = LowerExpression(*conditional.conditions[0].expr);
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

    case StatementKind::ForLoop: {
      const auto& for_loop = statement.as<slang::ast::ForLoopStatement>();

      // Note: loopVars contains variables declared in the for-loop header
      // (e.g., `for (int i = 0; ...)`). Slang emits these as separate
      // VariableDeclaration statements in the enclosing block, so we don't
      // need to process them here. We only process the initializers array,
      // which contains assignment expressions for pre-declared variables.
      std::vector<std::unique_ptr<mir::Statement>> initializers;
      for (const auto* expr : for_loop.initializers) {
        initializers.push_back(LowerExpressionStatement(*expr));
      }

      // Condition (nullptr if no stop expression = infinite loop)
      auto condition = (for_loop.stopExpr != nullptr)
                           ? LowerExpression(*for_loop.stopExpr)
                           : nullptr;

      // Step expressions
      std::vector<std::unique_ptr<mir::Expression>> steps;
      for (const auto* step : for_loop.steps) {
        steps.push_back(LowerExpression(*step));
      }

      // Body
      auto body = LowerStatement(for_loop.body);

      return std::make_unique<mir::ForStatement>(
          std::move(initializers), std::move(condition), std::move(steps),
          std::move(body));
    }

    case StatementKind::ForeverLoop: {
      const auto& forever_loop =
          statement.as<slang::ast::ForeverLoopStatement>();
      auto body = LowerStatement(forever_loop.body);

      // Create a constant true condition for the while loop
      auto true_condition =
          std::make_unique<mir::LiteralExpression>(common::Literal::Bool(true));

      // Create a while statement with the true condition
      return std::make_unique<mir::WhileStatement>(
          std::move(true_condition), std::move(body));
    }

    case StatementKind::Break: {
      return std::make_unique<mir::BreakStatement>();
    }

    case StatementKind::Continue: {
      return std::make_unique<mir::ContinueStatement>();
    }

    case StatementKind::Empty: {
      return std::make_unique<mir::BlockStatement>();  // No-op
    }

    default:
      throw DiagnosticException(
          Diagnostic::Error(
              statement.sourceRange,
              fmt::format(
                  "unsupported statement kind '{}'",
                  slang::ast::toString(statement.kind))));
  }
}

auto LowerVariableDeclaration(const slang::ast::VariableSymbol& symbol)
    -> std::unique_ptr<mir::VariableDeclarationStatement> {
  // Create source range from symbol location
  slang::SourceRange source_range(symbol.location, symbol.location);

  auto type_result = LowerType(symbol.getType(), source_range);
  if (!type_result) {
    throw DiagnosticException(std::move(type_result.error()));
  }

  common::Variable variable{
      .symbol = &symbol,
      .type = *type_result,
  };

  const auto* initializer = symbol.getInitializer();

  if (initializer != nullptr) {
    auto lowered_initializer = LowerExpression(*initializer);
    return std::make_unique<mir::VariableDeclarationStatement>(
        std::move(variable), std::move(lowered_initializer));
  }

  return std::make_unique<mir::VariableDeclarationStatement>(
      std::move(variable), nullptr);
}

auto LowerExpressionStatement(const slang::ast::Expression& expr)
    -> std::unique_ptr<mir::ExpressionStatement> {
  auto lowered_expr = LowerExpression(expr);
  return std::make_unique<mir::ExpressionStatement>(std::move(lowered_expr));
}

}  // namespace lyra::lowering::ast_to_mir
