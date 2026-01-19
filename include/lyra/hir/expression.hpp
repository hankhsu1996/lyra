#pragma once

#include <optional>
#include <variant>
#include <vector>

#include "lyra/common/constant.hpp"
#include "lyra/common/source_span.hpp"
#include "lyra/common/symbol_types.hpp"
#include "lyra/common/type.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/hir/operator.hpp"
#include "lyra/hir/system_call.hpp"

namespace lyra::hir {

enum class ExpressionKind {
  kConstant,
  kNameRef,
  kUnaryOp,
  kBinaryOp,
  kCast,
  kSystemCall,
  kConditional,
  kAssignment,
  kElementAccess,
  kMemberAccess,
  kStructLiteral,
  kCall,
  kNewArray,
  kBuiltinMethodCall,
};

struct ConstantExpressionData {
  ConstId constant;

  auto operator==(const ConstantExpressionData&) const -> bool = default;
};

struct NameRefExpressionData {
  SymbolId symbol;

  auto operator==(const NameRefExpressionData&) const -> bool = default;
};

struct UnaryExpressionData {
  UnaryOp op;
  ExpressionId operand;

  auto operator==(const UnaryExpressionData&) const -> bool = default;
};

struct BinaryExpressionData {
  BinaryOp op;
  ExpressionId lhs;
  ExpressionId rhs;

  auto operator==(const BinaryExpressionData&) const -> bool = default;
};

struct CastExpressionData {
  ExpressionId operand;

  auto operator==(const CastExpressionData&) const -> bool = default;
};

struct ConditionalExpressionData {
  ExpressionId condition;
  ExpressionId then_expr;
  ExpressionId else_expr;

  auto operator==(const ConditionalExpressionData&) const -> bool = default;
};

struct AssignmentExpressionData {
  ExpressionId target;
  ExpressionId value;

  auto operator==(const AssignmentExpressionData&) const -> bool = default;
};

struct ElementAccessExpressionData {
  ExpressionId base;
  ExpressionId index;

  auto operator==(const ElementAccessExpressionData&) const -> bool = default;
};

struct MemberAccessExpressionData {
  ExpressionId base;
  int field_index;

  auto operator==(const MemberAccessExpressionData&) const -> bool = default;
};

struct StructLiteralExpressionData {
  std::vector<ExpressionId> field_values;  // Declaration order

  auto operator==(const StructLiteralExpressionData&) const -> bool = default;
};

struct CallExpressionData {
  SymbolId callee;
  std::vector<ExpressionId> arguments;

  auto operator==(const CallExpressionData&) const -> bool = default;
};

enum class BuiltinMethod {
  kSize,
  kDelete,
};

struct NewArrayExpressionData {
  ExpressionId size_expr;
  std::optional<ExpressionId> init_expr;

  auto operator==(const NewArrayExpressionData&) const -> bool = default;
};

struct BuiltinMethodCallExpressionData {
  ExpressionId receiver;
  BuiltinMethod method;

  auto operator==(const BuiltinMethodCallExpressionData&) const
      -> bool = default;
};

using ExpressionData = std::variant<
    ConstantExpressionData, NameRefExpressionData, UnaryExpressionData,
    BinaryExpressionData, CastExpressionData, SystemCallExpressionData,
    ConditionalExpressionData, AssignmentExpressionData,
    ElementAccessExpressionData, MemberAccessExpressionData,
    StructLiteralExpressionData, CallExpressionData, NewArrayExpressionData,
    BuiltinMethodCallExpressionData>;

struct Expression {
  ExpressionKind kind;
  TypeId type;
  SourceSpan span;
  ExpressionData data;

  auto operator==(const Expression&) const -> bool = default;
};

}  // namespace lyra::hir
