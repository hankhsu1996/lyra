#include "lowering/ast_to_mir/expression.hpp"

#include <stdexcept>

#include <fmt/format.h>
#include <slang/ast/Expression.h>
#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/expressions/LiteralExpressions.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/expressions/OperatorExpressions.h>
#include <spdlog/spdlog.h>

#include "mir/expression.hpp"

namespace lyra::lowering {

auto LowerExpression(const slang::ast::Expression& expression)
    -> std::shared_ptr<mir::Expression> {
  switch (expression.kind) {
    case slang::ast::ExpressionKind::IntegerLiteral: {
      int value = expression.as<slang::ast::IntegerLiteral>()
                      .getValue()
                      .as<int>()
                      .value();
      return std::make_shared<mir::LiteralExpression>(value);
    }

    case slang::ast::ExpressionKind::NamedValue: {
      const auto& named_value =
          expression.as<slang::ast::NamedValueExpression>();
      return std::make_shared<mir::IdentifierExpression>(
          std::string(named_value.symbol.name));
    }

    case slang::ast::ExpressionKind::BinaryOp: {
      const auto& bin = expression.as<slang::ast::BinaryExpression>();
      switch (bin.op) {
        case slang::ast::BinaryOperator::Add: {
          auto left = LowerExpression(bin.left());
          auto right = LowerExpression(bin.right());
          return std::make_shared<mir::BinaryExpression>(
              mir::BinaryExpression::Operator::kAdd, std::move(left),
              std::move(right));
        }

        default:
          throw std::runtime_error(fmt::format(
              "Unsupported binary operator {} in LowerExpression",
              slang::ast::toString(bin.op)));
      }
    }

    case slang::ast::ExpressionKind::Assignment: {
      const auto& assignment =
          expression.as<slang::ast::AssignmentExpression>();
      const auto& left = assignment.left();

      if (left.kind != slang::ast::ExpressionKind::NamedValue) {
        throw std::runtime_error(fmt::format(
            "Unsupported assignment target kind {} in LowerExpression",
            slang::ast::toString(left.kind)));
      }

      auto target_name =
          left.as<slang::ast::NamedValueExpression>().symbol.name;
      auto value = LowerExpression(assignment.right());

      return std::make_shared<mir::AssignmentExpression>(
          std::string(target_name), std::move(value));
    }

    default:
      throw std::runtime_error("Unsupported expression kind");
  }
}

auto LowerExpressionFromName(const std::string& name)
    -> std::shared_ptr<mir::Expression> {
  return std::make_shared<mir::IdentifierExpression>(name);
}

}  // namespace lyra::lowering
