#pragma once

#include <cassert>
#include <cstdint>
#include <memory>
#include <ostream>
#include <stdexcept>
#include <string>
#include <vector>

#include <fmt/core.h>
#include <fmt/format.h>
#include <fmt/ranges.h>

#include "lyra/common/builtin_method.hpp"
#include "lyra/common/constant.hpp"
#include "lyra/common/hierarchical_path.hpp"
#include "lyra/common/symbol.hpp"
#include "lyra/common/type.hpp"
#include "lyra/mir/operators.hpp"
#include "lyra/mir/visitor.hpp"

namespace lyra::mir {

using Type = common::Type;
using Constant = common::Constant;
using SymbolRef = common::SymbolRef;
using HierarchicalPathElement = common::HierarchicalPathElement;
using common::FormatHierarchicalPath;

// Re-export from common for convenience
using BuiltinMethod = common::BuiltinMethod;
using common::ParseBuiltinMethod;
using common::ToString;

class Expression {
 public:
  enum class Kind {
    kConstant,
    kIdentifier,
    kEnumValue,
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
    kConcatenation,
    kReplication,
    kFunctionCall,
    kMemberAccess,
    kNewArray,
    kMethodCall,
    kUnpackedStructLiteral,
    kArrayLiteral,
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
    case Expression::Kind::kConstant:
      return "Constant";
    case Expression::Kind::kIdentifier:
      return "Identifier";
    case Expression::Kind::kEnumValue:
      return "EnumValue";
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
    case Expression::Kind::kConcatenation:
      return "Concatenation";
    case Expression::Kind::kReplication:
      return "Replication";
    case Expression::Kind::kFunctionCall:
      return "FunctionCall";
    case Expression::Kind::kMemberAccess:
      return "MemberAccess";
    case Expression::Kind::kNewArray:
      return "NewArray";
    case Expression::Kind::kMethodCall:
      return "MethodCall";
    case Expression::Kind::kUnpackedStructLiteral:
      return "UnpackedStructLiteral";
    case Expression::Kind::kArrayLiteral:
      return "ArrayLiteral";
  }
  std::abort();
}

inline auto operator<<(std::ostream& os, const Expression::Kind& kind)
    -> std::ostream& {
  return os << ToString(kind);
}

class ConstantExpression : public Expression {
 public:
  static constexpr Kind kKindValue = Kind::kConstant;

  Constant constant;

  explicit ConstantExpression(Constant constant)
      : Expression(kKindValue, constant.type), constant(std::move(constant)) {
  }

  [[nodiscard]] auto ToString() const -> std::string override {
    return constant.ToString();
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

// Represents an enum value reference (e.g., State::IDLE)
// Carries both the integer value (for interpreter) and names (for codegen)
class EnumValueExpression : public Expression {
 public:
  static constexpr Kind kKindValue = Kind::kEnumValue;
  std::string enum_name;   // "state_t"
  std::string value_name;  // "IDLE"
  int64_t value;           // 0

  EnumValueExpression(
      Type type, std::string enum_name, std::string value_name, int64_t value)
      : Expression(kKindValue, std::move(type)),
        enum_name(std::move(enum_name)),
        value_name(std::move(value_name)),
        value(value) {
  }

  [[nodiscard]] auto ToString() const -> std::string override {
    return fmt::format("{}::{}:{}", enum_name, value_name, type);
  }

  void Accept(MirVisitor& visitor) const override {
    visitor.Visit(*this);
  }
};

// Enum member info for MethodCallExpression on enum types
struct EnumMemberInfo {
  std::string name;
  int64_t value;
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
      std::unique_ptr<Expression> right, Type result_type)
      : Expression(kKindValue, std::move(result_type)),
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

// Represents one step in a field access path (e.g., s.inner.x has two elements)
// Used for nested struct/union field assignment
struct FieldPathElement {
  std::string name;  // Field name
  size_t index;      // Storage index (field index for unpacked, 0 for unions)
  Type type;         // Type of this field

  // For packed structs only (bit-level access)
  std::optional<uint64_t> bit_offset;  // LSB position within parent
  std::optional<size_t> bit_width;     // Width in bits

  // Constructor for unpacked struct/union field
  FieldPathElement(std::string name, size_t index, Type type)
      : name(std::move(name)), index(index), type(std::move(type)) {
  }

  // Constructor for packed struct field
  FieldPathElement(
      std::string name, uint64_t bit_offset, size_t bit_width, Type type)
      : name(std::move(name)),
        index(0),
        type(std::move(type)),
        bit_offset(bit_offset),
        bit_width(bit_width) {
  }

  [[nodiscard]] auto IsPacked() const -> bool {
    return bit_offset.has_value();
  }
};

// Represents the target of an assignment:
// - Local variable (symbol only)
// - Array element (symbol + indices for multi-dim packed arrays)
// - Hierarchical reference (path like "child.port" or "gen_block[0].signal")
// - Struct/union field (symbol + field_path for nested access)
struct AssignmentTarget {
  SymbolRef symbol;  // The base variable (nullptr for hierarchical)
  std::vector<std::unique_ptr<Expression>>
      indices;                    // Indices for element select (empty = simple)
  std::optional<Type> base_type;  // Type of base variable (for element select)

  // For hierarchical targets
  SymbolRef target_symbol{nullptr};
  std::vector<HierarchicalPathElement> instance_path;

  // For struct/union field assignment (supports nested: s.inner.x = value)
  std::vector<FieldPathElement> field_path;  // Empty = not a field assignment

  // Constructor for simple variable assignment
  explicit AssignmentTarget(SymbolRef sym)
      : symbol(std::move(sym)), indices(), base_type(std::nullopt) {
  }

  // Constructor for single-index element select assignment
  AssignmentTarget(SymbolRef sym, std::unique_ptr<Expression> index)
      : symbol(std::move(sym)), indices(), base_type(std::nullopt) {
    if (index) {
      indices.push_back(std::move(index));
    }
  }

  // Constructor for single-index element select assignment with base type
  AssignmentTarget(SymbolRef sym, std::unique_ptr<Expression> index, Type type)
      : symbol(std::move(sym)), indices(), base_type(std::move(type)) {
    if (index) {
      indices.push_back(std::move(index));
    }
  }

  // Constructor for multi-index element select assignment with base type
  AssignmentTarget(
      SymbolRef sym, std::vector<std::unique_ptr<Expression>> idxs, Type type)
      : symbol(std::move(sym)),
        indices(std::move(idxs)),
        base_type(std::move(type)) {
  }

  // Constructor for hierarchical reference assignment
  AssignmentTarget(
      SymbolRef target, std::vector<HierarchicalPathElement> instances)
      : symbol(nullptr),
        indices(),
        target_symbol(target),
        instance_path(std::move(instances)) {
  }

  // Constructor for struct/union field assignment (single level)
  AssignmentTarget(
      SymbolRef sym, std::string name, size_t field_index, Type field_type,
      Type struct_type)
      : symbol(std::move(sym)), indices(), base_type(std::move(struct_type)) {
    field_path.emplace_back(
        std::move(name), field_index, std::move(field_type));
  }

  // Constructor for struct/union field assignment (nested path)
  AssignmentTarget(
      SymbolRef sym, std::vector<FieldPathElement> path, Type struct_type)
      : symbol(std::move(sym)),
        indices(),
        base_type(std::move(struct_type)),
        field_path(std::move(path)) {
  }

  [[nodiscard]] auto IsElementSelect() const -> bool {
    return !indices.empty();
  }

  [[nodiscard]] auto IsPacked() const -> bool {
    return base_type && base_type->kind == Type::Kind::kIntegral;
  }

  [[nodiscard]] auto IsHierarchical() const -> bool {
    return target_symbol != nullptr;
  }

  [[nodiscard]] auto IsStructFieldAssignment() const -> bool {
    return !field_path.empty();
  }

  [[nodiscard]] auto ToString() const -> std::string {
    if (IsHierarchical()) {
      return FormatHierarchicalPath(instance_path, target_symbol);
    }
    if (IsStructFieldAssignment()) {
      std::string result = std::string(symbol->name);
      for (const auto& elem : field_path) {
        result += fmt::format(".{}", elem.name);
      }
      return result;
    }
    if (indices.empty()) {
      return std::string(symbol->name);
    }
    std::string result = std::string(symbol->name);
    for (const auto& idx : indices) {
      result += fmt::format("[{}]", idx->ToString());
    }
    return result;
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

  // For display-like tasks ($display, $monitor, $strobe, $error, etc.):
  // Optional format string expression. nullopt means no explicit format string
  // (all arguments are values to display with auto-format).
  // For non-display tasks: always nullopt.
  std::optional<std::unique_ptr<Expression>> format_expr;

  // For display-like tasks: values to format (the arguments after format_expr).
  // For non-display tasks: general arguments.
  std::vector<std::unique_ptr<Expression>> arguments;
  std::vector<AssignmentTarget> output_targets;

  // True if format_expr (or first argument for mem_io tasks) is a string
  // literal. For display tasks: enables compile-time format parsing. For
  // mem_io tasks ($readmemh, etc.): enables filename extraction from integral.
  bool format_expr_is_literal = false;

  // Source location for severity tasks ($fatal, $error, $warning, $info).
  // Captured at AST lowering time since slang source info is only available
  // there.
  std::optional<std::string> source_file;
  std::optional<uint32_t> source_line;

  SystemCallExpression(
      std::string name, std::vector<std::unique_ptr<Expression>> args,
      Type return_type)
      : Expression(kKindValue, std::move(return_type)),
        name(std::move(name)),
        arguments(std::move(args)) {
  }

  SystemCallExpression(
      std::string name, std::vector<std::unique_ptr<Expression>> args,
      std::vector<AssignmentTarget> outputs, Type return_type)
      : Expression(kKindValue, std::move(return_type)),
        name(std::move(name)),
        arguments(std::move(args)),
        output_targets(std::move(outputs)) {
  }

  [[nodiscard]] auto ToString() const -> std::string override {
    std::vector<std::string> arg_strs;
    if (format_expr) {
      arg_strs.push_back((*format_expr)->ToString());
    }
    arg_strs.reserve(arg_strs.size() + arguments.size());
    for (const auto& arg : arguments) {
      arg_strs.push_back(arg ? arg->ToString() : "<null>");
    }
    for (const auto& target : output_targets) {
      arg_strs.push_back("out:" + target.ToString());
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

// Represents a general hierarchical reference: path.to.signal or
// gen_block[0].signal Can be used as both RHS (reading) and LHS (writing via
// AssignmentTarget)
class HierarchicalReferenceExpression : public Expression {
 public:
  static constexpr Kind kKindValue = Kind::kHierarchicalReference;

  SymbolRef target_symbol;                             // Target variable symbol
  std::vector<HierarchicalPathElement> instance_path;  // Instance traversal

  HierarchicalReferenceExpression(
      SymbolRef target, std::vector<HierarchicalPathElement> instances,
      Type type)
      : Expression(kKindValue, std::move(type)),
        target_symbol(target),
        instance_path(std::move(instances)) {
  }

  [[nodiscard]] auto ToString() const -> std::string override {
    return FormatHierarchicalPath(instance_path, target_symbol);
  }

  void Accept(MirVisitor& visitor) const override {
    visitor.Visit(*this);
  }
};

// Concatenation expression: {a, b, c}
// Operands are ordered MSB to LSB (first operand is most significant)
// Result type is unsigned packed vector with width = sum of operand widths
class ConcatenationExpression : public Expression {
 public:
  static constexpr Kind kKindValue = Kind::kConcatenation;

  std::vector<std::unique_ptr<Expression>> operands;

  ConcatenationExpression(
      std::vector<std::unique_ptr<Expression>> ops, Type result_type)
      : Expression(kKindValue, std::move(result_type)),
        operands(std::move(ops)) {
  }

  [[nodiscard]] auto ToString() const -> std::string override {
    std::vector<std::string> op_strs;
    op_strs.reserve(operands.size());
    for (const auto& op : operands) {
      op_strs.push_back(op->ToString());
    }
    return fmt::format("{{{}}}", fmt::join(op_strs, ", "));
  }

  void Accept(MirVisitor& visitor) const override {
    visitor.Visit(*this);
  }
};

// Replication expression: {n{expr}}
// Replicates the operand n times. Result width = operand width * count.
// Note: In slang AST, operand is wrapped in a Concatenation node even for
// single operands.
class ReplicationExpression : public Expression {
 public:
  static constexpr Kind kKindValue = Kind::kReplication;

  std::unique_ptr<Expression> operand;  // Expression to replicate
  size_t count;                         // Number of replications

  ReplicationExpression(
      std::unique_ptr<Expression> op, size_t count, Type result_type)
      : Expression(kKindValue, std::move(result_type)),
        operand(std::move(op)),
        count(count) {
  }

  [[nodiscard]] auto ToString() const -> std::string override {
    return fmt::format("{{{}{{{}}}}}", count, operand->ToString());
  }

  void Accept(MirVisitor& visitor) const override {
    visitor.Visit(*this);
  }
};

// User-defined function call expression
// Distinguished from SystemCallExpression which handles $display, $time, etc.
// function_name holds the full qualified name (e.g., "MyPkg::add" for package
// functions, or just "add" for module-local functions).
class FunctionCallExpression : public Expression {
 public:
  static constexpr Kind kKindValue = Kind::kFunctionCall;

  std::string function_name;  // Full qualified name (e.g., "MyPkg::add")
  std::vector<std::unique_ptr<Expression>> arguments;

  FunctionCallExpression(
      std::string name, std::vector<std::unique_ptr<Expression>> args,
      Type return_type)
      : Expression(kKindValue, std::move(return_type)),
        function_name(std::move(name)),
        arguments(std::move(args)) {
  }

  [[nodiscard]] auto ToString() const -> std::string override {
    std::vector<std::string> arg_strs;
    arg_strs.reserve(arguments.size());
    for (const auto& arg : arguments) {
      arg_strs.push_back(arg ? arg->ToString() : "<null>");
    }
    return fmt::format("{}({})", function_name, fmt::join(arg_strs, ", "));
  }

  void Accept(MirVisitor& visitor) const override {
    visitor.Visit(*this);
  }
};

// Member access expression for packed structs: struct_val.field_name
// At MIR level, we preserve the field metadata for downstream phases
// At LIR level, this becomes SliceRef/LoadSlice (addressable) or ExtractBits
// (rvalue)
class MemberAccessExpression : public Expression {
 public:
  static constexpr Kind kKindValue = Kind::kMemberAccess;

  std::unique_ptr<Expression> value;  // The struct being accessed
  std::string field_name;             // Name of the field
  uint64_t bit_offset;                // LSB position within the struct
  size_t bit_width;                   // Width of the field in bits

  MemberAccessExpression(
      std::unique_ptr<Expression> value, std::string field_name,
      uint64_t bit_offset, size_t bit_width, Type field_type)
      : Expression(kKindValue, std::move(field_type)),
        value(std::move(value)),
        field_name(std::move(field_name)),
        bit_offset(bit_offset),
        bit_width(bit_width) {
  }

  [[nodiscard]] auto ToString() const -> std::string override {
    return fmt::format("{}.{}", value->ToString(), field_name);
  }

  void Accept(MirVisitor& visitor) const override {
    visitor.Visit(*this);
  }
};

// Dynamic array new[] constructor: new[size] or new[size](init)
class NewArrayExpression : public Expression {
 public:
  static constexpr Kind kKindValue = Kind::kNewArray;

  std::unique_ptr<Expression> size_expr;  // Required: array size
  std::unique_ptr<Expression> init_expr;  // Optional: initializer array

  NewArrayExpression(
      Type type, std::unique_ptr<Expression> size,
      std::unique_ptr<Expression> init)
      : Expression(kKindValue, std::move(type)),
        size_expr(std::move(size)),
        init_expr(std::move(init)) {
  }

  [[nodiscard]] auto ToString() const -> std::string override {
    if (init_expr) {
      return fmt::format(
          "new[{}]({})", size_expr->ToString(), init_expr->ToString());
    }
    return fmt::format("new[{}]", size_expr->ToString());
  }

  void Accept(MirVisitor& visitor) const override {
    visitor.Visit(*this);
  }
};

// Unified method call expression for all built-in type methods.
// The receiver's type determines the method semantics.
//
// Design note: enum_members is only populated for enum receivers. This couples
// enum-specific data to the unified expression, but avoids the complexity of
// std::variant for receiver-type-specific data. The tradeoff is acceptable
// because: (1) only enums need extra data for codegen, (2) empty vector has
// minimal overhead, (3) simpler than a visitor pattern for 2 receiver types.
class MethodCallExpression : public Expression {
 public:
  static constexpr Kind kKindValue = Kind::kMethodCall;

  std::unique_ptr<Expression> receiver;
  BuiltinMethod method;
  std::vector<std::unique_ptr<Expression>> args;

  // Only populated for enum receivers (needed for switch codegen)
  std::vector<EnumMemberInfo> enum_members;

  MethodCallExpression(
      Type type, std::unique_ptr<Expression> receiver, BuiltinMethod method,
      std::vector<std::unique_ptr<Expression>> args = {},
      std::vector<EnumMemberInfo> enum_members = {})
      : Expression(kKindValue, std::move(type)),
        receiver(std::move(receiver)),
        method(method),
        args(std::move(args)),
        enum_members(std::move(enum_members)) {
  }

  [[nodiscard]] auto ToString() const -> std::string override {
    if (args.empty()) {
      return fmt::format(
          "{}.{}()", receiver->ToString(), mir::ToString(method));
    }
    std::vector<std::string> arg_strs;
    arg_strs.reserve(args.size());
    for (const auto& arg : args) {
      arg_strs.push_back(arg->ToString());
    }
    return fmt::format(
        "{}.{}({})", receiver->ToString(), mir::ToString(method),
        fmt::join(arg_strs, ", "));
  }

  void Accept(MirVisitor& visitor) const override {
    visitor.Visit(*this);
  }
};

// Unpacked struct literal expression: '{field0, field1, ...}
// Creates an unpacked struct value from field values in declaration order.
class UnpackedStructLiteralExpression : public Expression {
 public:
  static constexpr Kind kKindValue = Kind::kUnpackedStructLiteral;

  std::vector<std::unique_ptr<Expression>> field_values;

  UnpackedStructLiteralExpression(
      Type type, std::vector<std::unique_ptr<Expression>> field_values)
      : Expression(kKindValue, std::move(type)),
        field_values(std::move(field_values)) {
  }

  [[nodiscard]] auto ToString() const -> std::string override {
    std::vector<std::string> value_strs;
    value_strs.reserve(field_values.size());
    for (const auto& v : field_values) {
      value_strs.push_back(v->ToString());
    }
    return fmt::format("'{{{}}}", fmt::join(value_strs, ", "));
  }

  void Accept(MirVisitor& visitor) const override {
    visitor.Visit(*this);
  }
};

// Array/queue literal expression: '{elem0, elem1, ...}
// Creates a dynamic array or queue from element values.
class ArrayLiteralExpression : public Expression {
 public:
  static constexpr Kind kKindValue = Kind::kArrayLiteral;

  std::vector<std::unique_ptr<Expression>> elements;

  ArrayLiteralExpression(
      Type type, std::vector<std::unique_ptr<Expression>> elements)
      : Expression(kKindValue, std::move(type)), elements(std::move(elements)) {
  }

  [[nodiscard]] auto ToString() const -> std::string override {
    std::vector<std::string> elem_strs;
    elem_strs.reserve(elements.size());
    for (const auto& e : elements) {
      elem_strs.push_back(e->ToString());
    }
    return fmt::format("'{{{}}}", fmt::join(elem_strs, ", "));
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
