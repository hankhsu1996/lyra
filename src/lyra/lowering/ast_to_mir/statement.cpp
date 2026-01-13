#include "lyra/lowering/ast_to_mir/statement.hpp"

#include <cstddef>
#include <cstdint>
#include <memory>
#include <utility>
#include <vector>

#include <fmt/format.h>
#include <slang/ast/Expression.h>
#include <slang/ast/SemanticFacts.h>
#include <slang/ast/Statement.h>
#include <slang/ast/TimingControl.h>
#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/expressions/CallExpression.h>
#include <slang/ast/expressions/LiteralExpressions.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/statements/ConditionalStatements.h>
#include <slang/ast/statements/LoopStatements.h>
#include <slang/ast/statements/MiscStatements.h>
#include <slang/ast/symbols/VariableSymbols.h>

#include "lyra/common/constant.hpp"
#include "lyra/common/diagnostic.hpp"
#include "lyra/common/trigger.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/common/variable.hpp"
#include "lyra/lowering/ast_to_mir/expression.hpp"
#include "lyra/lowering/ast_to_mir/literal.hpp"
#include "lyra/lowering/ast_to_mir/type.hpp"
#include "lyra/mir/expression.hpp"
#include "lyra/mir/operators.hpp"
#include "lyra/mir/statement.hpp"

namespace lyra::lowering::ast_to_mir {

using StatementKind = slang::ast::StatementKind;
using ExpressionKind = slang::ast::ExpressionKind;
using TimingControlKind = slang::ast::TimingControlKind;

namespace {

// Extract a Trigger from a SignalEventControl
auto ExtractTrigger(const slang::ast::SignalEventControl& signal_event)
    -> common::Trigger {
  const auto& expr = signal_event.expr;

  if (expr.kind != ExpressionKind::NamedValue) {
    throw DiagnosticException(
        Diagnostic::Error(
            expr.sourceRange,
            "only simple identifier supported in signal event expression"));
  }

  const auto& named_expr = expr.as<slang::ast::NamedValueExpression>();
  const auto& variable = named_expr.symbol;

  common::EdgeKind edge_kind{};
  switch (signal_event.edge) {
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

  return common::Trigger{
      .edge_kind = edge_kind, .variable = &variable, .instance_path = {}};
}

// Convert slang's UniquePriorityCheck to MIR's enum
auto ConvertUniquePriorityCheck(slang::ast::UniquePriorityCheck check)
    -> mir::UniquePriorityCheck {
  switch (check) {
    case slang::ast::UniquePriorityCheck::None:
      return mir::UniquePriorityCheck::kNone;
    case slang::ast::UniquePriorityCheck::Unique:
      return mir::UniquePriorityCheck::kUnique;
    case slang::ast::UniquePriorityCheck::Unique0:
      return mir::UniquePriorityCheck::kUnique0;
    case slang::ast::UniquePriorityCheck::Priority:
      return mir::UniquePriorityCheck::kPriority;
  }
}

// Lower a foreach loop to equivalent nested for loops.
// For multi-dimensional arrays like `foreach(arr[i,j,k])`, generates:
//   { int i; int j; int k;
//     for (i = ...) { for (j = ...) { for (k = ...) { body } } } }
// Skipped dimensions like `foreach(arr[i,,k])` generate no loop for that
// dimension.
auto LowerForeachLoop(
    const slang::ast::ForeachLoopStatement& foreach_loop,
    common::TypeArena& arena) -> std::unique_ptr<mir::Statement> {
  // Check for empty loopDims (shouldn't happen, but be safe)
  if (foreach_loop.loopDims.empty()) {
    throw DiagnosticException(
        Diagnostic::Error(
            foreach_loop.sourceRange, "foreach loop has no dimensions"));
  }

  // Create outer block to hold all variable declarations and nested for loops
  auto block = std::make_unique<mir::BlockStatement>();

  // Declare loop variables upfront (skip dimensions with no loop variable)
  for (const auto& dim : foreach_loop.loopDims) {
    if (dim.loopVar == nullptr) {
      continue;
    }
    auto var_decl = LowerVariableDeclaration(*dim.loopVar, arena);
    block->statements.push_back(std::move(var_decl));
  }

  // Lower the body statement once
  std::unique_ptr<mir::Statement> current =
      LowerStatement(foreach_loop.body, arena);

  // Build nested for loops from innermost to outermost
  // Process dimensions in reverse order so the first dimension is outermost
  // Skip dimensions with no loop variable (foreach(arr[i,,k]) skips middle)
  for (auto i = static_cast<int>(foreach_loop.loopDims.size()) - 1; i >= 0;
       --i) {
    const auto& dim = foreach_loop.loopDims[static_cast<size_t>(i)];
    if (dim.loopVar == nullptr) {
      continue;
    }
    const auto* loop_var = dim.loopVar;

    // Get the iterator variable type
    slang::SourceRange source_range(loop_var->location, loop_var->location);
    auto type_result = LowerType(loop_var->getType(), source_range, arena);
    if (!type_result) {
      throw DiagnosticException(std::move(type_result.error()));
    }
    const auto& var_type = *type_result;

    std::vector<std::unique_ptr<mir::Statement>> initializers;
    std::unique_ptr<mir::Expression> condition;
    std::vector<std::unique_ptr<mir::Expression>> steps;

    if (dim.range.has_value()) {
      // Static dimension: use compile-time bounds from range
      const auto& range = dim.range.value();

      // Determine iteration direction and bounds
      // isLittleEndian() returns true when left >= right (e.g., [3:0])
      bool is_descending = range.isLittleEndian();
      int32_t start_value = range.left;
      int32_t end_value = range.right;

      // Create initializer: var = start_value
      auto start_constant = std::make_unique<mir::ConstantExpression>(
          common::Constant::IntegralSigned(start_value, 32));
      auto init_assign = std::make_unique<mir::AssignmentExpression>(
          loop_var, std::move(start_constant), false);
      initializers.push_back(
          std::make_unique<mir::ExpressionStatement>(std::move(init_assign)));

      // Create condition: var >= end (descending) or var <= end (ascending)
      auto cond_var_ref =
          std::make_unique<mir::IdentifierExpression>(var_type, loop_var);
      auto end_constant = std::make_unique<mir::ConstantExpression>(
          common::Constant::IntegralSigned(end_value, 32));
      mir::BinaryOperator cmp_op = is_descending
                                       ? mir::BinaryOperator::kGreaterThanEqual
                                       : mir::BinaryOperator::kLessThanEqual;
      condition = std::make_unique<mir::BinaryExpression>(
          cmp_op, std::move(cond_var_ref), std::move(end_constant),
          common::Type::Bool());

      // Create step: var-- (descending) or var++ (ascending)
      auto step_var_ref =
          std::make_unique<mir::IdentifierExpression>(var_type, loop_var);
      mir::UnaryOperator step_op = is_descending
                                       ? mir::UnaryOperator::kPostdecrement
                                       : mir::UnaryOperator::kPostincrement;
      steps.push_back(
          std::make_unique<mir::UnaryExpression>(
              step_op, std::move(step_var_ref)));
    } else {
      // Dynamic dimension: iterate from 0 to size()-1
      // Generates: for (i = 0; i < arr.size(); i++)

      // Create initializer: var = 0
      auto start_constant = std::make_unique<mir::ConstantExpression>(
          common::Constant::IntegralSigned(0, 32));
      auto init_assign = std::make_unique<mir::AssignmentExpression>(
          loop_var, std::move(start_constant), false);
      initializers.push_back(
          std::make_unique<mir::ExpressionStatement>(std::move(init_assign)));

      // Create condition: var < arr.size()
      auto cond_var_ref =
          std::make_unique<mir::IdentifierExpression>(var_type, loop_var);

      // Synthesize size() method call on the array
      auto array_ref = LowerExpression(foreach_loop.arrayRef, arena);
      auto size_call = std::make_unique<mir::MethodCallExpression>(
          common::Type::Int(), std::move(array_ref), mir::BuiltinMethod::kSize);

      condition = std::make_unique<mir::BinaryExpression>(
          mir::BinaryOperator::kLessThan, std::move(cond_var_ref),
          std::move(size_call), common::Type::Bool());

      // Create step: var++
      auto step_var_ref =
          std::make_unique<mir::IdentifierExpression>(var_type, loop_var);
      steps.push_back(
          std::make_unique<mir::UnaryExpression>(
              mir::UnaryOperator::kPostincrement, std::move(step_var_ref)));
    }

    // Create ForStatement wrapping current content
    auto for_stmt = std::make_unique<mir::ForStatement>(
        std::move(initializers), std::move(condition), std::move(steps),
        std::move(current));

    current = std::move(for_stmt);
  }

  block->statements.push_back(std::move(current));
  return block;
}

}  // namespace

auto LowerStatement(
    const slang::ast::Statement& statement, common::TypeArena& arena)
    -> std::unique_ptr<mir::Statement> {
  switch (statement.kind) {
    case StatementKind::List: {
      const auto& statement_list = statement.as<slang::ast::StatementList>();
      auto block = std::make_unique<mir::BlockStatement>();
      for (const auto* statement : statement_list.list) {
        block->statements.push_back(LowerStatement(*statement, arena));
      }
      return block;
    }

    case StatementKind::Block: {
      const auto& block_statement = statement.as<slang::ast::BlockStatement>();
      return LowerStatement(block_statement.body, arena);
    }

    case StatementKind::VariableDeclaration: {
      const auto& declaration =
          statement.as<slang::ast::VariableDeclStatement>();
      return LowerVariableDeclaration(declaration.symbol, arena);
    }

    case StatementKind::ExpressionStatement: {
      const auto& expression_statement =
          statement.as<slang::ast::ExpressionStatement>();
      return LowerExpressionStatement(expression_statement.expr, arena);
    }

    case StatementKind::Timed: {
      const auto& timed_statement = statement.as<slang::ast::TimedStatement>();
      const auto& timing_control = timed_statement.timing;

      switch (timing_control.kind) {
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
          auto inner_statement = LowerStatement(timed_statement.stmt, arena);

          auto block = std::make_unique<mir::BlockStatement>();
          block->statements.push_back(std::move(delay_statement));
          block->statements.push_back(std::move(inner_statement));
          return block;
        }

        case slang::ast::TimingControlKind::SignalEvent: {
          const auto& signal_event_control =
              timing_control.as<slang::ast::SignalEventControl>();
          std::vector<common::Trigger> triggers = {
              ExtractTrigger(signal_event_control)};

          auto wait_statement =
              std::make_unique<mir::WaitEventStatement>(std::move(triggers));
          auto inner_statement = LowerStatement(timed_statement.stmt, arena);

          auto block = std::make_unique<mir::BlockStatement>();
          block->statements.push_back(std::move(wait_statement));
          block->statements.push_back(std::move(inner_statement));
          return block;
        }

        case slang::ast::TimingControlKind::EventList: {
          const auto& event_list_control =
              timing_control.as<slang::ast::EventListControl>();

          std::vector<common::Trigger> triggers;
          triggers.reserve(event_list_control.events.size());

          for (const auto* event : event_list_control.events) {
            if (event->kind != TimingControlKind::SignalEvent) {
              throw DiagnosticException(
                  Diagnostic::Error(
                      event->sourceRange,
                      fmt::format(
                          "only signal events supported in event list, got: {}",
                          slang::ast::toString(event->kind))));
            }
            const auto& signal_event =
                event->as<slang::ast::SignalEventControl>();
            triggers.push_back(ExtractTrigger(signal_event));
          }

          auto wait_statement =
              std::make_unique<mir::WaitEventStatement>(std::move(triggers));
          auto inner_statement = LowerStatement(timed_statement.stmt, arena);

          auto block = std::make_unique<mir::BlockStatement>();
          block->statements.push_back(std::move(wait_statement));
          block->statements.push_back(std::move(inner_statement));
          return block;
        }
        case slang::ast::TimingControlKind::Invalid:
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

      auto condition = LowerExpression(*conditional.conditions[0].expr, arena);
      auto then_branch = LowerStatement(conditional.ifTrue, arena);

      std::unique_ptr<mir::Statement> else_branch;
      if (conditional.ifFalse != nullptr) {
        else_branch = LowerStatement(*conditional.ifFalse, arena);
      }

      auto check = ConvertUniquePriorityCheck(conditional.check);

      return std::make_unique<mir::ConditionalStatement>(
          std::move(condition), std::move(then_branch), std::move(else_branch),
          check);
    }

    case StatementKind::WhileLoop: {
      const auto& while_loop = statement.as<slang::ast::WhileLoopStatement>();
      auto condition = LowerExpression(while_loop.cond, arena);
      auto body = LowerStatement(while_loop.body, arena);
      return std::make_unique<mir::WhileStatement>(
          std::move(condition), std::move(body));
    }

    case StatementKind::DoWhileLoop: {
      const auto& do_while_loop =
          statement.as<slang::ast::DoWhileLoopStatement>();
      auto condition = LowerExpression(do_while_loop.cond, arena);
      auto body = LowerStatement(do_while_loop.body, arena);
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
        initializers.push_back(LowerExpressionStatement(*expr, arena));
      }

      // Condition (nullptr if no stop expression = infinite loop)
      auto condition = (for_loop.stopExpr != nullptr)
                           ? LowerExpression(*for_loop.stopExpr, arena)
                           : nullptr;

      // Step expressions
      std::vector<std::unique_ptr<mir::Expression>> steps;
      for (const auto* step : for_loop.steps) {
        steps.push_back(LowerExpression(*step, arena));
      }

      // Body
      auto body = LowerStatement(for_loop.body, arena);

      return std::make_unique<mir::ForStatement>(
          std::move(initializers), std::move(condition), std::move(steps),
          std::move(body));
    }

    case StatementKind::ForeverLoop: {
      const auto& forever_loop =
          statement.as<slang::ast::ForeverLoopStatement>();
      auto body = LowerStatement(forever_loop.body, arena);

      // Create a constant true condition for the while loop
      auto true_condition = std::make_unique<mir::ConstantExpression>(
          common::Constant::Bool(true));

      // Create a while statement with the true condition
      return std::make_unique<mir::WhileStatement>(
          std::move(true_condition), std::move(body));
    }

    case StatementKind::RepeatLoop: {
      const auto& repeat_loop = statement.as<slang::ast::RepeatLoopStatement>();
      auto count = LowerExpression(repeat_loop.count, arena);
      auto body = LowerStatement(repeat_loop.body, arena);
      return std::make_unique<mir::RepeatStatement>(
          std::move(count), std::move(body));
    }

    case StatementKind::ForeachLoop: {
      const auto& foreach_loop =
          statement.as<slang::ast::ForeachLoopStatement>();
      return LowerForeachLoop(foreach_loop, arena);
    }

    case StatementKind::Case: {
      const auto& case_stmt = statement.as<slang::ast::CaseStatement>();

      // Convert slang condition to MIR CaseCondition
      mir::CaseCondition case_condition{};
      switch (case_stmt.condition) {
        case slang::ast::CaseStatementCondition::Normal:
          case_condition = mir::CaseCondition::kNormal;
          break;
        case slang::ast::CaseStatementCondition::WildcardJustZ:
          case_condition = mir::CaseCondition::kWildcardZ;
          break;
        case slang::ast::CaseStatementCondition::WildcardXOrZ:
          case_condition = mir::CaseCondition::kWildcardXZ;
          break;
        case slang::ast::CaseStatementCondition::Inside:
          throw DiagnosticException(
              Diagnostic::Error(
                  statement.sourceRange, "case inside is not yet supported"));
      }

      // Lower the controlling expression
      auto condition = LowerExpression(case_stmt.expr, arena);

      // Lower each case item
      std::vector<mir::CaseItem> items;
      for (const auto& item : case_stmt.items) {
        // Lower all expressions and extract masks for this item
        std::vector<std::unique_ptr<mir::Expression>> exprs;
        std::vector<int64_t> masks;

        for (const auto* expr : item.expressions) {
          // For casez/casex, extract mask from constant patterns
          if (case_condition != mir::CaseCondition::kNormal) {
            // Get constant value - slang evaluates constant expressions
            const auto* const_val = expr->getConstant();
            if (const_val == nullptr || !const_val->isInteger()) {
              throw DiagnosticException(
                  Diagnostic::Error(
                      expr->sourceRange,
                      "casez/casex patterns must be compile-time constants"));
            }

            const auto& sv_int = const_val->integer();
            auto [value, mask] = ExtractMaskAndValue(sv_int, case_condition);

            // Create a literal expression with the masked value
            // Match the signedness of the case expression
            auto width = sv_int.getBitWidth();
            auto constant = sv_int.isSigned()
                                ? common::Constant::IntegralSigned(value, width)
                                : common::Constant::IntegralUnsigned(
                                      static_cast<uint64_t>(value), width);
            exprs.push_back(
                std::make_unique<mir::ConstantExpression>(constant));
            masks.push_back(mask);
          } else {
            // Normal case - use regular expression lowering
            exprs.push_back(LowerExpression(*expr, arena));
            // Mask of all 1s for normal case (compare all bits)
            masks.push_back(-1);
          }
        }
        // Lower the statement
        auto stmt = LowerStatement(*item.stmt, arena);
        items.emplace_back(std::move(exprs), std::move(masks), std::move(stmt));
      }

      // Lower optional default case
      std::unique_ptr<mir::Statement> default_case;
      if (case_stmt.defaultCase != nullptr) {
        default_case = LowerStatement(*case_stmt.defaultCase, arena);
      }

      // Convert unique/priority qualifier
      auto check = ConvertUniquePriorityCheck(case_stmt.check);

      return std::make_unique<mir::CaseStatement>(
          case_condition, std::move(condition), std::move(items),
          std::move(default_case), check);
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

    case StatementKind::Return: {
      const auto& return_stmt = statement.as<slang::ast::ReturnStatement>();

      std::unique_ptr<mir::Expression> return_value = nullptr;
      if (return_stmt.expr != nullptr) {
        return_value = LowerExpression(*return_stmt.expr, arena);
      }

      return std::make_unique<mir::ReturnStatement>(std::move(return_value));
    }

    case StatementKind::Invalid:
      // Slang produces InvalidStatement when it detects semantic issues.
      // Slang should have already reported a diagnostic explaining the problem.
      // We cannot proceed with invalid AST nodes.
      throw DiagnosticException(
          Diagnostic::Error({}, "cannot lower invalid statement"));

    case StatementKind::ProceduralAssign:
    case StatementKind::ProceduralDeassign:
      throw DiagnosticException(
          Diagnostic::Error(
              statement.sourceRange,
              "procedural continuous assignment (assign/deassign) is not "
              "supported"));

    default:
      throw DiagnosticException(
          Diagnostic::Error(
              statement.sourceRange,
              fmt::format(
                  "unsupported statement kind '{}'",
                  slang::ast::toString(statement.kind))));
  }
}

auto LowerVariableDeclaration(
    const slang::ast::VariableSymbol& symbol, common::TypeArena& arena)
    -> std::unique_ptr<mir::VariableDeclarationStatement> {
  // Create source range from symbol location
  slang::SourceRange source_range(symbol.location, symbol.location);

  auto type_result = LowerType(symbol.getType(), source_range, arena);
  if (!type_result) {
    throw DiagnosticException(std::move(type_result.error()));
  }

  common::Variable variable{
      .symbol = &symbol,
      .type = *type_result,
  };

  const auto* initializer = symbol.getInitializer();

  if (initializer != nullptr) {
    auto lowered_initializer = LowerExpression(*initializer, arena);
    return std::make_unique<mir::VariableDeclarationStatement>(
        std::move(variable), std::move(lowered_initializer));
  }

  return std::make_unique<mir::VariableDeclarationStatement>(
      std::move(variable), nullptr);
}

auto LowerExpressionStatement(
    const slang::ast::Expression& expr, common::TypeArena& arena)
    -> std::unique_ptr<mir::ExpressionStatement> {
  auto lowered_expr = LowerExpression(expr, arena);
  return std::make_unique<mir::ExpressionStatement>(std::move(lowered_expr));
}

}  // namespace lyra::lowering::ast_to_mir
