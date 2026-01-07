#pragma once

#include <cassert>
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
    kElementSelect,
    kRangeSelect,
    kIndexedRangeSelect,
    kHierarchicalReference,
  };

  Kind kind;
  Type type;

  Expression(const Expression&) = default;
  Expression(Expression&&) = delete;
  auto operator=(const Expression&) -> Expression& = default;
  auto operator=(Expression&&) -> Expression& = delete;
  explicit Expression(Kind kind, Type type)
      : kind(kind), type(std::move(type)) {
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
    case Expression::Kind::kElementSelect:
      return "ElementSelect";
    case Expression::Kind::kRangeSelect:
      return "RangeSelect";
    case Expression::Kind::kIndexedRangeSelect:
      return "IndexedRangeSelect";
    case Expression::Kind::kHierarchicalReference:
      return "HierarchicalReference";
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
    return fmt::format(
        "({} ? {} : {})", condition->ToString(), true_expression->ToString(),
        false_expression->ToString());
  }

  void Accept(MirVisitor& visitor) const override {
    visitor.Visit(*this);
  }
};

// Forward declaration for AssignmentTarget
class ElementSelectExpression;

// Represents the target of an assignment:
// - Local variable (symbol only)
// - Array element (symbol + index)
// - Hierarchical reference (path like "child.port")
struct AssignmentTarget {
  // For local symbol targets
  SymbolRef symbol;  // The base variable (nullptr for hierarchical)
  std::unique_ptr<Expression>
      element_index;              // Optional: index for element select
  std::optional<Type> base_type;  // Type of base variable (for element select)

  // For hierarchical targets: symbol-based (for interpreter)
  SymbolRef hier_target_symbol{nullptr};
  std::vector<SymbolRef> hier_instance_symbols;

  // For hierarchical targets: string-based (for codegen)
  std::vector<std::string> hierarchical_path;

  // Constructor for simple variable assignment
  explicit AssignmentTarget(SymbolRef sym)
      : symbol(std::move(sym)),
        element_index(nullptr),
        base_type(std::nullopt) {
  }

  // Constructor for element select assignment
  AssignmentTarget(SymbolRef sym, std::unique_ptr<Expression> index)
      : symbol(std::move(sym)),
        element_index(std::move(index)),
        base_type(std::nullopt) {
  }

  // Constructor for element select assignment with base type
  AssignmentTarget(SymbolRef sym, std::unique_ptr<Expression> index, Type type)
      : symbol(std::move(sym)),
        element_index(std::move(index)),
        base_type(std::move(type)) {
  }

  // Constructor for hierarchical reference assignment (symbol-based)
  AssignmentTarget(
      SymbolRef target, std::vector<SymbolRef> instances,
      std::vector<std::string> path)
      : symbol(nullptr),
        element_index(nullptr),
        hier_target_symbol(target),
        hier_instance_symbols(std::move(instances)),
        hierarchical_path(std::move(path)) {
    assert(!hierarchical_path.empty() && "Hierarchical path must not be empty");
  }

  [[nodiscard]] auto IsElementSelect() const -> bool {
    return element_index != nullptr;
  }

  [[nodiscard]] auto IsPacked() const -> bool {
    return base_type && base_type->kind == Type::Kind::kIntegral;
  }

  [[nodiscard]] auto IsHierarchical() const -> bool {
    return hier_target_symbol != nullptr;
  }

  [[nodiscard]] auto ToString() const -> std::string {
    if (IsHierarchical()) {
      return fmt::format("{}", fmt::join(hierarchical_path, "."));
    }
    if (element_index) {
      return fmt::format("{}[{}]", symbol->name, element_index->ToString());
    }
    return std::string(symbol->name);
  }
};

class AssignmentExpression : public Expression {
 public:
  static constexpr Kind kKindValue = Kind::kAssignment;
  AssignmentTarget target;
  std::shared_ptr<Expression> value;
  bool is_non_blocking;

  AssignmentExpression(
      AssignmentTarget target, std::shared_ptr<Expression> v,
      bool is_non_blocking)
      : Expression(kKindValue, v->type),
        target(std::move(target)),
        value(std::move(v)),
        is_non_blocking(is_non_blocking) {
  }

  // Convenience constructor for simple variable assignment
  AssignmentExpression(
      SymbolRef sym, std::shared_ptr<Expression> v, bool is_non_blocking)
      : Expression(kKindValue, v->type),
        target(std::move(sym)),
        value(std::move(v)),
        is_non_blocking(is_non_blocking) {
  }

  [[nodiscard]] auto ToString() const -> std::string override {
    return fmt::format(
        "({} = {})", target.ToString(), value ? value->ToString() : "<null>");
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

class ElementSelectExpression : public Expression {
 public:
  static constexpr Kind kKindValue = Kind::kElementSelect;

  std::unique_ptr<Expression> value;     // Array being indexed
  std::unique_ptr<Expression> selector;  // Index expression

  ElementSelectExpression(
      std::unique_ptr<Expression> value, std::unique_ptr<Expression> selector,
      Type element_type)
      : Expression(kKindValue, std::move(element_type)),
        value(std::move(value)),
        selector(std::move(selector)) {
  }

  [[nodiscard]] auto ToString() const -> std::string override {
    return fmt::format("{}[{}]", value->ToString(), selector->ToString());
  }

  void Accept(MirVisitor& visitor) const override {
    visitor.Visit(*this);
  }
};

class RangeSelectExpression : public Expression {
 public:
  static constexpr Kind kKindValue = Kind::kRangeSelect;

  std::unique_ptr<Expression> value;  // Packed vector being sliced
  int32_t left;                       // Left bound (e.g., 7 in a[7:4])
  int32_t right;                      // Right bound (e.g., 4 in a[7:4])

  RangeSelectExpression(
      std::unique_ptr<Expression> value, int32_t left, int32_t right,
      Type result_type)
      : Expression(kKindValue, std::move(result_type)),
        value(std::move(value)),
        left(left),
        right(right) {
  }

  [[nodiscard]] auto ToString() const -> std::string override {
    return fmt::format("{}[{}:{}]", value->ToString(), left, right);
  }

  void Accept(MirVisitor& visitor) const override {
    visitor.Visit(*this);
  }
};

// Indexed part-select: a[i+:4] or a[i-:4]
class IndexedRangeSelectExpression : public Expression {
 public:
  static constexpr Kind kKindValue = Kind::kIndexedRangeSelect;

  std::unique_ptr<Expression> value;  // Packed vector being sliced
  std::unique_ptr<Expression> start;  // Starting index (variable)
  bool is_ascending;                  // true for +: (IndexedUp), false for -:
  int32_t width;                      // Constant width

  IndexedRangeSelectExpression(
      std::unique_ptr<Expression> value, std::unique_ptr<Expression> start,
      bool is_ascending, int32_t width, Type result_type)
      : Expression(kKindValue, std::move(result_type)),
        value(std::move(value)),
        start(std::move(start)),
        is_ascending(is_ascending),
        width(width) {
  }

  [[nodiscard]] auto ToString() const -> std::string override {
    return fmt::format(
        "{}[{}{}:{}]", value->ToString(), start->ToString(),
        is_ascending ? "+" : "-", width);
  }

  void Accept(MirVisitor& visitor) const override {
    visitor.Visit(*this);
  }
};

// Represents a general hierarchical reference: path.to.signal
// Can be used as both RHS (reading) and LHS (writing via AssignmentTarget)
class HierarchicalReferenceExpression : public Expression {
 public:
  static constexpr Kind kKindValue = Kind::kHierarchicalReference;

  // For interpreter: symbol pointers (globally unique, no string lookup needed)
  SymbolRef target_symbol;                  // Target variable symbol
  std::vector<SymbolRef> instance_symbols;  // Instance traversal path

  // For codegen: string path (needed for C++ code generation)
  std::vector<std::string> path;  // e.g., ["child", "a"]

  HierarchicalReferenceExpression(
      SymbolRef target, std::vector<SymbolRef> instances,
      std::vector<std::string> path, Type type)
      : Expression(kKindValue, std::move(type)),
        target_symbol(target),
        instance_symbols(std::move(instances)),
        path(std::move(path)) {
    assert(!this->path.empty() && "Hierarchical path must not be empty");
  }

  [[nodiscard]] auto ToString() const -> std::string override {
    return fmt::format("{}", fmt::join(path, "."));
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
