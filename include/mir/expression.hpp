#pragma once

#include <memory>
#include <ostream>
#include <stdexcept>
#include <string>
#include <vector>

#include <fmt/core.h>
#include <fmt/format.h>
#include <fmt/ranges.h>

namespace lyra::mir {

class Expression {
 public:
  enum class Kind {
    kLiteral,
    kIdentifier,
    kBinary,
    kAssignment,
    kSystemCall,
  };

  Kind kind;

  Expression(const Expression&) = default;
  Expression(Expression&&) = delete;
  auto operator=(const Expression&) -> Expression& = default;
  auto operator=(Expression&&) -> Expression& = delete;
  explicit Expression(Kind kind) : kind(kind) {
  }
  virtual ~Expression() = default;

  [[nodiscard]] virtual auto ToString() const -> std::string = 0;
};

// Convert Expression::Kind to string
inline auto ToString(Expression::Kind kind) -> std::string {
  switch (kind) {
    case Expression::Kind::kLiteral:
      return "Literal";
    case Expression::Kind::kIdentifier:
      return "Identifier";
    case Expression::Kind::kBinary:
      return "Binary";
    case Expression::Kind::kAssignment:
      return "Assignment";
    case Expression::Kind::kSystemCall:
      return "SystemCall";
  }
}

// Add operator<< for Expression::Kind
inline auto operator<<(std::ostream& os, const Expression::Kind& kind)
    -> std::ostream& {
  return os << ToString(kind);
}

class LiteralExpression : public Expression {
 public:
  static constexpr Kind kKindValue = Kind::kLiteral;
  int value;

  explicit LiteralExpression(int v) : Expression(Kind::kLiteral), value(v) {
  }

  [[nodiscard]] auto ToString() const -> std::string override {
    return fmt::format("{}", value);
  }
};

class IdentifierExpression : public Expression {
 public:
  static constexpr Kind kKindValue = Kind::kIdentifier;
  std::string name;

  explicit IdentifierExpression(std::string n)
      : Expression(Kind::kIdentifier), name(std::move(n)) {
  }

  [[nodiscard]] auto ToString() const -> std::string override {
    return name;
  }
};

class BinaryExpression : public Expression {
 public:
  static constexpr Kind kKindValue = Kind::kBinary;
  enum class Operator { kAdd };

  Operator op;
  std::unique_ptr<Expression> left;
  std::unique_ptr<Expression> right;

  BinaryExpression(
      Operator op, std::unique_ptr<Expression> left,
      std::unique_ptr<Expression> right)
      : Expression(Kind::kBinary),
        op(op),
        left(std::move(left)),
        right(std::move(right)) {
  }

  [[nodiscard]] auto ToString() const -> std::string override {
    if (!left || !right) {
      return "(invalid binary expression)";
    }

    switch (op) {
      case Operator::kAdd:
        return fmt::format("({} + {})", left->ToString(), right->ToString());
      default:
        return "(unknown operator)";
    }
  }
};

// Convert BinaryExpression::Operator to string
inline auto ToString(BinaryExpression::Operator op) -> std::string {
  switch (op) {
    case BinaryExpression::Operator::kAdd:
      return "Add";
  }
}

// Add operator<< for BinaryExpression::Operator
inline auto operator<<(std::ostream& os, const BinaryExpression::Operator& op)
    -> std::ostream& {
  return os << ToString(op);
}

class AssignmentExpression : public Expression {
 public:
  static constexpr Kind kKindValue = Kind::kAssignment;
  std::string target;
  std::shared_ptr<Expression> value;

  AssignmentExpression(std::string t, std::shared_ptr<Expression> v)
      : Expression(Kind::kAssignment),
        target(std::move(t)),
        value(std::move(v)) {
  }

  [[nodiscard]] auto ToString() const -> std::string override {
    if (!value) {
      return fmt::format("({} = <null>)", target);
    }
    return fmt::format("({} = {})", target, value->ToString());
  }
};

class SystemCallExpression : public Expression {
 public:
  static constexpr Kind kKindValue = Kind::kSystemCall;
  std::string name;
  std::vector<std::unique_ptr<Expression>> arguments;

  SystemCallExpression(
      std::string name, std::vector<std::unique_ptr<Expression>> args)
      : Expression(Kind::kSystemCall),
        name(std::move(name)),
        arguments(std::move(args)) {};

  [[nodiscard]] auto ToString() const -> std::string override {
    std::vector<std::string> arg_strs;
    arg_strs.reserve(arguments.size());
    for (const auto& arg : arguments) {
      arg_strs.push_back(arg ? arg->ToString() : "<null>");
    }
    return fmt::format("({} {})", name, fmt::join(arg_strs, ", "));
  }
};

// Helper function for safely casting expressions
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

// Add formatter for Expression::Kind enum
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

// Add formatter for BinaryExpression::Operator
template <>
struct fmt::formatter<lyra::mir::BinaryExpression::Operator> {
  template <typename ParseContext>
  constexpr auto parse(ParseContext& ctx) {
    return ctx.begin();
  }

  template <typename FormatContext>
  auto format(
      const lyra::mir::BinaryExpression::Operator& op,
      FormatContext& ctx) const {
    return fmt::format_to(ctx.out(), "{}", lyra::mir::ToString(op));
  }
};
