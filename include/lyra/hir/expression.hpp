#pragma once

#include <optional>
#include <variant>
#include <vector>

#include "lyra/common/constant.hpp"
#include "lyra/common/math_fn.hpp"
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
  kBitCast,  // Bit reinterpretation (preserves bit pattern)
  kSystemCall,
  kConditional,
  kAssignment,
  kCompoundAssignment,
  kElementAccess,
  kMemberAccess,
  kUnionMemberAccess,
  kStructLiteral,
  kArrayLiteral,
  kCall,
  kNewArray,
  kBuiltinMethodCall,
  kPackedElementSelect,
  kPackedFieldAccess,  // s.field on packed struct
  kBitSelect,          // x[i] on integral (single bit extraction)
  kRangeSelect,        // x[a:b] constant range
  kIndexedPartSelect,  // x[i +: w] or x[i -: w]
  kConcat,             // {a, b, c} packed concatenation
  kReplicate,          // {N{expr}} replication (compact form)
  kHierarchicalRef,  // Hierarchical path reference (resolved to target symbol)
  kMathCall,         // IEEE 1800 20.8 math function call
  kMaterializeInitializer,  // Initializer pattern materialized as expression
};

// Returns true if the expression kind can appear as an assignment target.
inline auto IsPlaceExpressionKind(ExpressionKind kind) -> bool {
  switch (kind) {
    case ExpressionKind::kNameRef:
    case ExpressionKind::kElementAccess:
    case ExpressionKind::kMemberAccess:
    case ExpressionKind::kPackedElementSelect:
    case ExpressionKind::kPackedFieldAccess:
    case ExpressionKind::kBitSelect:
    case ExpressionKind::kRangeSelect:
    case ExpressionKind::kIndexedPartSelect:
    case ExpressionKind::kHierarchicalRef:
      return true;
    default:
      return false;
  }
}

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

struct BitCastExpressionData {
  ExpressionId operand;
  // Target type is expr.type (same pattern as CastExpressionData)

  auto operator==(const BitCastExpressionData&) const -> bool = default;
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
  bool is_non_blocking = false;

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

struct UnionMemberAccessExpressionData {
  ExpressionId base;
  int member_index = 0;

  auto operator==(const UnionMemberAccessExpressionData&) const
      -> bool = default;
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
  // Enum methods (runtime only - first/last/num are constant-folded)
  kEnumNext,
  kEnumPrev,
  kEnumName,
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

struct IndexedPartSelectExpressionData {
  ExpressionId base;       // The base integral/packed value
  ExpressionId index;      // Dynamic base index expression
  uint32_t width = 0;      // Static width (compile-time constant)
  bool ascending = false;  // true for +: , false for -:

  auto operator==(const IndexedPartSelectExpressionData&) const
      -> bool = default;
};

struct ConcatExpressionData {
  std::vector<ExpressionId> operands;  // MSB to LSB order

  auto operator==(const ConcatExpressionData&) const -> bool = default;
};

struct ReplicateExpressionData {
  ExpressionId element;
  uint32_t count;

  auto operator==(const ReplicateExpressionData&) const -> bool = default;
};

struct HierarchicalRefExpressionData {
  SymbolId target;  // Resolved variable in elaborated instance body

  auto operator==(const HierarchicalRefExpressionData&) const -> bool = default;
};

struct MathCallExpressionData {
  MathFn fn;
  std::vector<ExpressionId> args;

  auto operator==(const MathCallExpressionData&) const -> bool = default;
};

// Materialize an initializer pattern as an expression value.
// This is the only way initializers appear in expression position.
// At HIR->MIR lowering, creates a temp, calls LowerPattern, returns Use(temp).
struct MaterializeInitializerExpressionData {
  PatternId pattern;
  // target_type comes from expr.type

  auto operator==(const MaterializeInitializerExpressionData&) const
      -> bool = default;
};

using ExpressionData = std::variant<
    ConstantExpressionData, NameRefExpressionData, UnaryExpressionData,
    BinaryExpressionData, CastExpressionData, BitCastExpressionData,
    SystemCallExpressionData, ConditionalExpressionData,
    AssignmentExpressionData, CompoundAssignmentExpressionData,
    ElementAccessExpressionData, MemberAccessExpressionData,
    UnionMemberAccessExpressionData, StructLiteralExpressionData,
    ArrayLiteralExpressionData, CallExpressionData, NewArrayExpressionData,
    BuiltinMethodCallExpressionData, PackedElementSelectExpressionData,
    PackedFieldAccessExpressionData, BitSelectExpressionData,
    RangeSelectExpressionData, IndexedPartSelectExpressionData,
    ConcatExpressionData, ReplicateExpressionData,
    HierarchicalRefExpressionData, MathCallExpressionData,
    MaterializeInitializerExpressionData>;

struct Expression {
  ExpressionKind kind;
  TypeId type;
  SourceSpan span;
  ExpressionData data;

  auto operator==(const Expression&) const -> bool = default;
};

}  // namespace lyra::hir
