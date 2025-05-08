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

namespace lyra::lowering {

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

    case slang::ast::ExpressionKind::NamedValue: {
      const auto& named_value =
          expression.as<slang::ast::NamedValueExpression>();
      auto type = LowerType(named_value.symbol.getType());
      auto symbol = std::cref(named_value.symbol);
      return std::make_unique<mir::IdentifierExpression>(
          std::string(named_value.symbol.name), type, symbol);
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
      assert(left->type == right->type);

      auto mir_operator =
          mir::ConvertSlangBinaryOperatorToMir(binary_expression.op);

      return std::make_unique<mir::BinaryExpression>(
          mir_operator, std::move(left), std::move(right));
    }

    case slang::ast::ExpressionKind::Assignment: {
      const auto& assignment =
          expression.as<slang::ast::AssignmentExpression>();
      const auto& left = assignment.left();

      if (left.kind != slang::ast::ExpressionKind::NamedValue) {
        throw std::runtime_error(fmt::format(
            "Unsupported assignment target kind {} in AST to MIR "
            "LowerExpression",
            slang::ast::toString(left.kind)));
      }

      auto target_name =
          left.as<slang::ast::NamedValueExpression>().symbol.name;
      auto value = LowerExpression(assignment.right());
      auto is_non_blocking = assignment.isNonBlocking();
      return std::make_unique<mir::AssignmentExpression>(
          std::string(target_name), std::move(value), is_non_blocking);
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

      throw std::runtime_error(fmt::format(
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
      throw std::runtime_error(fmt::format(
          "Unsupported expression kind {} in AST to MIR LowerExpression",
          slang::ast::toString(expression.kind)));
  }
}

}  // namespace lyra::lowering
