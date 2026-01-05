#include "lyra/lowering/ast_to_mir/expression.hpp"

#include <stdexcept>

#include <fmt/format.h>
#include <slang/ast/Expression.h>
#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/expressions/CallExpression.h>
#include <slang/ast/expressions/ConversionExpression.h>
#include <slang/ast/expressions/LiteralExpressions.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/expressions/OperatorExpressions.h>
#include <spdlog/spdlog.h>

#include "lyra/lowering/ast_to_mir/literal.hpp"
#include "lyra/lowering/ast_to_mir/type.hpp"
#include "lyra/mir/expression.hpp"

namespace lyra::lowering::ast_to_mir {

auto LowerExpression(const slang::ast::Expression& expression)
    -> std::unique_ptr<mir::Expression> {
  switch (expression.kind) {
    case slang::ast::ExpressionKind::IntegerLiteral: {
      const auto& literal = expression.as<slang::ast::IntegerLiteral>();
      auto mir_literal = LowerLiteral(literal);
      return std::make_unique<mir::LiteralExpression>(std::move(mir_literal));
    }

    case slang::ast::ExpressionKind::StringLiteral: {
      const auto& literal = expression.as<slang::ast::StringLiteral>();
      auto mir_literal = LowerLiteral(literal);
      return std::make_unique<mir::LiteralExpression>(std::move(mir_literal));
    }

    case slang::ast::ExpressionKind::RealLiteral: {
      const auto& literal = expression.as<slang::ast::RealLiteral>();
      auto mir_literal = LowerLiteral(literal);
      return std::make_unique<mir::LiteralExpression>(std::move(mir_literal));
    }

    case slang::ast::ExpressionKind::NamedValue: {
      const auto& named_value =
          expression.as<slang::ast::NamedValueExpression>();
      auto type = LowerType(named_value.symbol.getType());
      return std::make_unique<mir::IdentifierExpression>(
          type, &named_value.symbol);
    }

    case slang::ast::ExpressionKind::UnaryOp: {
      const auto& unary_expression =
          expression.as<slang::ast::UnaryExpression>();

      auto operand = LowerExpression(unary_expression.operand());
      auto mir_operator =
          mir::ConvertSlangUnaryOperatorToMir(unary_expression.op);

      return std::make_unique<mir::UnaryExpression>(
          mir_operator, std::move(operand));
    }

    case slang::ast::ExpressionKind::BinaryOp: {
      const auto& binary_expression =
          expression.as<slang::ast::BinaryExpression>();

      auto left = LowerExpression(binary_expression.left());
      auto right = LowerExpression(binary_expression.right());

      auto mir_operator =
          mir::ConvertSlangBinaryOperatorToMir(binary_expression.op);

      return std::make_unique<mir::BinaryExpression>(
          mir_operator, std::move(left), std::move(right));
    }

    case slang::ast::ExpressionKind::ConditionalOp: {
      const auto& conditional_expression =
          expression.as<slang::ast::ConditionalExpression>();

      if (conditional_expression.conditions.size() != 1) {
        throw std::runtime_error(
            fmt::format(
                "Unsupported conditional expression with {} conditions in AST "
                "to "
                "MIR "
                "LowerExpression",
                conditional_expression.conditions.size()));
      }

      auto condition =
          LowerExpression(*conditional_expression.conditions[0].expr);
      auto true_expression = LowerExpression(conditional_expression.left());
      auto false_expression = LowerExpression(conditional_expression.right());
      return std::make_unique<mir::TernaryExpression>(
          std::move(condition), std::move(true_expression),
          std::move(false_expression));
    }

    case slang::ast::ExpressionKind::Assignment: {
      const auto& assignment =
          expression.as<slang::ast::AssignmentExpression>();
      const auto& left = assignment.left();

      if (left.kind != slang::ast::ExpressionKind::NamedValue) {
        throw std::runtime_error(
            fmt::format(
                "Unsupported assignment target kind {} in AST to MIR "
                "LowerExpression",
                slang::ast::toString(left.kind)));
      }

      const auto& target = left.as<slang::ast::NamedValueExpression>().symbol;
      auto value = LowerExpression(assignment.right());
      auto is_non_blocking = assignment.isNonBlocking();
      return std::make_unique<mir::AssignmentExpression>(
          &target, std::move(value), is_non_blocking);
    }

    case slang::ast::ExpressionKind::Call: {
      const auto& call_expression = expression.as<slang::ast::CallExpression>();
      if (call_expression.isSystemCall()) {
        auto name = call_expression.getSubroutineName();
        std::vector<std::unique_ptr<mir::Expression>> arguments;
        for (const auto& arg : call_expression.arguments()) {
          arguments.push_back(LowerExpression(*arg));
        }
        auto return_type = LowerType(*call_expression.type);
        return std::make_unique<mir::SystemCallExpression>(
            std::string(name), std::move(arguments), return_type);
      }

      throw std::runtime_error(
          fmt::format(
              "Unsupported subroutine call {} in AST to MIR LowerExpression",
              call_expression.getSubroutineName()));
    }

    case slang::ast::ExpressionKind::Conversion: {
      const auto& conversion =
          expression.as<slang::ast::ConversionExpression>();
      auto value = LowerExpression(conversion.operand());
      auto type = LowerType(*conversion.type);
      return std::make_unique<mir::ConversionExpression>(
          std::move(value), type);
    }

    default:
      throw std::runtime_error(
          fmt::format(
              "Unsupported expression kind {} in AST to MIR LowerExpression",
              slang::ast::toString(expression.kind)));
  }
}

}  // namespace lyra::lowering::ast_to_mir
