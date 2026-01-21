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
  kCompoundAssignment,
  kElementAccess,
  kMemberAccess,
  kStructLiteral,
  kArrayLiteral,
  kCall,
  kNewArray,
  kBuiltinMethodCall,
  kPackedElementSelect,
  kPackedFieldAccess,  // s.field on packed struct
  kBitSelect,          // x[i] on integral (single bit extraction)
  kRangeSelect,        // x[a:b] constant range
  kConcat,             // {a, b, c} packed concatenation
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

struct CompoundAssignmentExpressionData {
  BinaryOp op;
  ExpressionId target;
  ExpressionId operand;  // RHS of compound op (e.g., 'b' in 'a += b')

  auto operator==(const CompoundAssignmentExpressionData&) const
      -> bool = default;
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

struct ArrayLiteralExpressionData {
  std::vector<ExpressionId> elements;

  auto operator==(const ArrayLiteralExpressionData&) const -> bool = default;
};

struct CallExpressionData {
  SymbolId callee;
  std::vector<ExpressionId> arguments;

  auto operator==(const CallExpressionData&) const -> bool = default;
};

enum class BuiltinMethod {
  kSize,
  kDelete,
  kPushBack,
  kPushFront,
  kPopBack,
  kPopFront,
  kInsert,
};

struct NewArrayExpressionData {
  ExpressionId size_expr;
  std::optional<ExpressionId> init_expr;

  auto operator==(const NewArrayExpressionData&) const -> bool = default;
};

struct BuiltinMethodCallExpressionData {
  ExpressionId receiver;
  BuiltinMethod method;
  std::vector<ExpressionId> args;

  auto operator==(const BuiltinMethodCallExpressionData&) const
      -> bool = default;
};

struct PackedElementSelectExpressionData {
  ExpressionId base;   // The packed array value (type carries bounds/direction)
  ExpressionId index;  // Element index

  auto operator==(const PackedElementSelectExpressionData&) const
      -> bool = default;
};

struct PackedFieldAccessExpressionData {
  ExpressionId base;    // The packed struct value
  int field_index;      // Index into struct fields
  uint32_t bit_offset;  // Pre-computed from type (from LSB)
  uint32_t bit_width;   // Pre-computed from type

  auto operator==(const PackedFieldAccessExpressionData&) const
      -> bool = default;
};

struct BitSelectExpressionData {
  ExpressionId base;   // The integral value
  ExpressionId index;  // Bit index

  auto operator==(const BitSelectExpressionData&) const -> bool = default;
};

struct RangeSelectExpressionData {
  ExpressionId base;  // The integral value
  int32_t left = 0;   // Left bound (source order)
  int32_t right = 0;  // Right bound (source order)

  auto operator==(const RangeSelectExpressionData&) const -> bool = default;
};

struct ConcatExpressionData {
  std::vector<ExpressionId> operands;  // MSB to LSB order

  auto operator==(const ConcatExpressionData&) const -> bool = default;
};

using ExpressionData = std::variant<
    ConstantExpressionData, NameRefExpressionData, UnaryExpressionData,
    BinaryExpressionData, CastExpressionData, SystemCallExpressionData,
    ConditionalExpressionData, AssignmentExpressionData,
    CompoundAssignmentExpressionData, ElementAccessExpressionData,
    MemberAccessExpressionData, StructLiteralExpressionData,
    ArrayLiteralExpressionData, CallExpressionData, NewArrayExpressionData,
    BuiltinMethodCallExpressionData, PackedElementSelectExpressionData,
    PackedFieldAccessExpressionData, BitSelectExpressionData,
    RangeSelectExpressionData, ConcatExpressionData>;

struct Expression {
  ExpressionKind kind;
  TypeId type;
  SourceSpan span;
  ExpressionData data;

  auto operator==(const Expression&) const -> bool = default;
};

}  // namespace lyra::hir
