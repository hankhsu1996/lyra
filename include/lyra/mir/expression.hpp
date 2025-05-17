#pragma once

#include <memory>
#include <ostream>
#include <stdexcept>
#include <string>
#include <vector>

#include <fmt/core.h>
#include <fmt/format.h>
#include <fmt/ranges.h>

#include "lyra/common/literal.hpp"
#include "lyra/common/symbol.hpp"
#include "lyra/common/type.hpp"
#include "lyra/mir/operators.hpp"
#include "lyra/mir/visitor.hpp"

namespace lyra::mir {

using Type = common::Type;
using Literal = common::Literal;
using SymbolRef = common::SymbolRef;

class Expression {
 public:
  enum class Kind {
    kLiteral,
    kIdentifier,
    kUnary,
    kBinary,
    kTernary,
    kAssignment,
    kConversion,
    kSystemCall,
  };

  Kind kind;
  Type type;

  Expression(const Expression&) = default;
  Expression(Expression&&) = delete;
  auto operator=(const Expression&) -> Expression& = default;
  auto operator=(Expression&&) -> Expression& = delete;
  explicit Expression(Kind kind, Type type) : kind(kind), type(type) {
  }
  virtual ~Expression() = default;

  [[nodiscard]] virtual auto ToString() const -> std::string = 0;
  virtual void Accept(MirVisitor& visitor) const = 0;
};

inline auto ToString(Expression::Kind kind) -> std::string {
  switch (kind) {
    case Expression::Kind::kLiteral:
      return "Literal";
    case Expression::Kind::kIdentifier:
      return "Identifier";
    case Expression::Kind::kUnary:
      return "Unary";
    case Expression::Kind::kBinary:
      return "Binary";
    case Expression::Kind::kTernary:
      return "Ternary";
    case Expression::Kind::kAssignment:
      return "Assignment";
    case Expression::Kind::kConversion:
      return "Conversion";
    case Expression::Kind::kSystemCall:
      return "SystemCall";
  }
  std::abort();
}

inline auto operator<<(std::ostream& os, const Expression::Kind& kind)
    -> std::ostream& {
  return os << ToString(kind);
}

class LiteralExpression : public Expression {
 public:
  static constexpr Kind kKindValue = Kind::kLiteral;

  Literal literal;

  explicit LiteralExpression(Literal literal)
      : Expression(kKindValue, literal.type), literal(std::move(literal)) {
  }

  [[nodiscard]] auto ToString() const -> std::string override {
    return literal.ToString();
  }

  void Accept(MirVisitor& visitor) const override {
    visitor.Visit(*this);
  }
};

class IdentifierExpression : public Expression {
 public:
  static constexpr Kind kKindValue = Kind::kIdentifier;
  SymbolRef symbol;

  IdentifierExpression(Type type, SymbolRef symbol)
      : Expression(kKindValue, type), symbol(symbol) {
  }

  [[nodiscard]] auto ToString() const -> std::string override {
    return fmt::format("{}:{}", symbol->name, type);
  }

  void Accept(MirVisitor& visitor) const override {
    visitor.Visit(*this);
  }
};

class UnaryExpression : public Expression {
 public:
  static constexpr Kind kKindValue = Kind::kUnary;
  UnaryOperator op;
  std::unique_ptr<Expression> operand;

  UnaryExpression(UnaryOperator op, std::unique_ptr<Expression> operand)
      : Expression(kKindValue, operand->type),
        op(op),
        operand(std::move(operand)) {
  }

  [[nodiscard]] auto ToString() const -> std::string override {
    return fmt::format("({} {})", op, operand->ToString());
  }

  void Accept(MirVisitor& visitor) const override {
    visitor.Visit(*this);
  }
};

class BinaryExpression : public Expression {
 public:
  static constexpr Kind kKindValue = Kind::kBinary;
  BinaryOperator op;
  std::unique_ptr<Expression> left;
  std::unique_ptr<Expression> right;

  BinaryExpression(
      BinaryOperator op, std::unique_ptr<Expression> left,
      std::unique_ptr<Expression> right)
      : Expression(kKindValue, left->type),
        op(op),
        left(std::move(left)),
        right(std::move(right)) {
  }

  [[nodiscard]] auto ToString() const -> std::string override {
    return fmt::format("({} {} {})", left->ToString(), op, right->ToString());
  }

  void Accept(MirVisitor& visitor) const override {
    visitor.Visit(*this);
  }
};

class TernaryExpression : public Expression {
 public:
  static constexpr Kind kKindValue = Kind::kTernary;
  std::unique_ptr<Expression> condition;
  std::unique_ptr<Expression> true_expression;
  std::unique_ptr<Expression> false_expression;

  TernaryExpression(
      std::unique_ptr<Expression> condition,
      std::unique_ptr<Expression> true_expression,
      std::unique_ptr<Expression> false_expression)
      : Expression(kKindValue, true_expression->type),
        condition(std::move(condition)),
        true_expression(std::move(true_expression)),
        false_expression(std::move(false_expression)) {
  }

  [[nodiscard]] auto ToString() const -> std::string override {
    return fmt::format("({} ? {} : {})", condition->ToString(),
                       true_expression->ToString(),
                       false_expression->ToString());
  }

  void Accept(MirVisitor& visitor) const override {
    visitor.Visit(*this);
  }
};


class AssignmentExpression : public Expression {
 public:
  static constexpr Kind kKindValue = Kind::kAssignment;
  SymbolRef target;
  std::shared_ptr<Expression> value;
  bool is_non_blocking;

  AssignmentExpression(
      SymbolRef target, std::shared_ptr<Expression> v, bool is_non_blocking)
      : Expression(kKindValue, v->type),
        target(target),
        value(std::move(v)),
        is_non_blocking(is_non_blocking) {
  }

  [[nodiscard]] auto ToString() const -> std::string override {
    return fmt::format(
        "({} = {})", target->name, value ? value->ToString() : "<null>");
  }

  void Accept(MirVisitor& visitor) const override {
    visitor.Visit(*this);
  }
};

class ConversionExpression : public Expression {
 public:
  static constexpr Kind kKindValue = Kind::kConversion;
  std::unique_ptr<Expression> value;
  Type target_type;

  ConversionExpression(std::unique_ptr<Expression> v, Type target_type)
      : Expression(kKindValue, target_type),
        value(std::move(v)),
        target_type(target_type) {
  }

  [[nodiscard]] auto ToString() const -> std::string override {
    return fmt::format("({} as {})", value->ToString(), target_type);
  }

  void Accept(MirVisitor& visitor) const override {
    visitor.Visit(*this);
  }
};

class SystemCallExpression : public Expression {
 public:
  static constexpr Kind kKindValue = Kind::kSystemCall;

  std::string name;
  std::vector<std::unique_ptr<Expression>> arguments;

  SystemCallExpression(
      std::string name, std::vector<std::unique_ptr<Expression>> args,
      Type return_type)
      : Expression(kKindValue, std::move(return_type)),
        name(std::move(name)),
        arguments(std::move(args)) {
  }

  [[nodiscard]] auto ToString() const -> std::string override {
    std::vector<std::string> arg_strs;
    arg_strs.reserve(arguments.size());
    for (const auto& arg : arguments) {
      arg_strs.push_back(arg ? arg->ToString() : "<null>");
    }
    return fmt::format("({} {})", name, fmt::join(arg_strs, ", "));
  }

  void Accept(MirVisitor& visitor) const override {
    visitor.Visit(*this);
  }
};

template <typename T>
auto As(const Expression& expr) -> const T& {
  if (expr.kind != T::kKindValue) {
    throw std::runtime_error("Expression kind mismatch in As<T>() cast");
  }
  return static_cast<const T&>(expr);
}

inline auto operator<<(std::ostream& os, const Expression& expr)
    -> std::ostream& {
  return os << expr.ToString();
}

}  // namespace lyra::mir

template <>
struct fmt::formatter<lyra::mir::Expression> {
  template <typename ParseContext>
  constexpr auto parse(ParseContext& ctx) {
    return ctx.begin();
  }

  template <typename FormatContext>
  auto format(const lyra::mir::Expression& expr, FormatContext& ctx) const {
    return fmt::format_to(ctx.out(), "{}", expr.ToString());
  }
};

template <>
struct fmt::formatter<lyra::mir::Expression::Kind> {
  template <typename ParseContext>
  constexpr auto parse(ParseContext& ctx) {
    return ctx.begin();
  }

  template <typename FormatContext>
  auto format(
      const lyra::mir::Expression::Kind& kind, FormatContext& ctx) const {
    return fmt::format_to(ctx.out(), "{}", lyra::mir::ToString(kind));
  }
};
