#include "lowering/ast_to_mir/expression.hpp"

#include <stdexcept>

#include <fmt/format.h>
#include <slang/ast/Expression.h>
#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/expressions/CallExpression.h>
#include <slang/ast/expressions/LiteralExpressions.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/expressions/OperatorExpressions.h>
#include <spdlog/spdlog.h>

#include "mir/expression.hpp"

namespace lyra::lowering {

auto LowerExpression(const slang::ast::Expression& expression)
    -> std::unique_ptr<mir::Expression> {
  switch (expression.kind) {
    case slang::ast::ExpressionKind::IntegerLiteral: {
      int value = expression.as<slang::ast::IntegerLiteral>()
                      .getValue()
                      .as<int>()
                      .value();
      return std::make_unique<mir::LiteralExpression>(value);
    }

    case slang::ast::ExpressionKind::NamedValue: {
      const auto& named_value =
          expression.as<slang::ast::NamedValueExpression>();
      return std::make_unique<mir::IdentifierExpression>(
          std::string(named_value.symbol.name));
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

      return std::make_unique<mir::AssignmentExpression>(
          std::string(target_name), std::move(value));
    }

    case slang::ast::ExpressionKind::Call: {
      const auto& call_expression = expression.as<slang::ast::CallExpression>();
      if (call_expression.isSystemCall()) {
        auto name = call_expression.getSubroutineName();
        std::vector<std::unique_ptr<mir::Expression>> arguments;
        for (const auto& arg : call_expression.arguments()) {
          arguments.push_back(LowerExpression(*arg));
        }
        return std::make_unique<mir::SystemCallExpression>(
            std::string(name), std::move(arguments));
      }

      throw std::runtime_error(fmt::format(
          "Unsupported subroutine call {} in AST to MIR LowerExpression",
          call_expression.getSubroutineName()));
    }

    default:
      throw std::runtime_error(fmt::format(
          "Unsupported expression kind {} in AST to MIR LowerExpression",
          slang::ast::toString(expression.kind)));
  }
}

auto LowerExpressionFromName(const std::string& name)
    -> std::unique_ptr<mir::Expression> {
  return std::make_unique<mir::IdentifierExpression>(name);
}

}  // namespace lyra::lowering
