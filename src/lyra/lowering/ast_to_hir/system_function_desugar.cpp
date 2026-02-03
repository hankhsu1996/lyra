#include "lyra/lowering/ast_to_hir/system_function_desugar.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <optional>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include <slang/ast/expressions/CallExpression.h>
#include <slang/ast/expressions/ConversionExpression.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/expressions/SelectExpressions.h>
#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/ParameterSymbols.h>
#include <slang/ast/types/AllTypes.h>
#include <slang/ast/types/TypePrinter.h>
#include <slang/numeric/Time.h>

#include "lyra/common/constant.hpp"
#include "lyra/common/constant_arena.hpp"
#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "lyra/common/integral_constant.hpp"
#include "lyra/common/math_fn.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/source_span.hpp"
#include "lyra/common/timescale_format.hpp"
#include "lyra/common/type.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/hir/expression.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/hir/system_call.hpp"
#include "lyra/lowering/ast_to_hir/context.hpp"
#include "lyra/lowering/ast_to_hir/detail/expression_lowering.hpp"
#include "lyra/lowering/ast_to_hir/timescale.hpp"
#include "lyra/lowering/ast_to_hir/type.hpp"
#include "lyra/semantic/bitcast_validation.hpp"
#include "lyra/semantic/signedness_cast.hpp"

namespace lyra::lowering::ast_to_hir {

auto ClassifyDesugarableSystemFunction(const slang::ast::CallExpression& call)
    -> std::optional<DesugarableClassification> {
  if (!call.isSystemCall()) {
    return std::nullopt;
  }

  std::string_view name = call.getSubroutineName();

  // Conversion functions
  if (name == "$signed") {
    return ConversionSysFnKind::kSigned;
  }
  if (name == "$unsigned") {
    return ConversionSysFnKind::kUnsigned;
  }
  if (name == "$itor") {
    return ConversionSysFnKind::kItor;
  }
  if (name == "$rtoi") {
    return ConversionSysFnKind::kRtoi;
  }
  if (name == "$realtobits") {
    return ConversionSysFnKind::kRealToBits;
  }
  if (name == "$bitstoreal") {
    return ConversionSysFnKind::kBitsToReal;
  }
  if (name == "$shortrealtobits") {
    return ConversionSysFnKind::kShortRealToBits;
  }
  if (name == "$bitstoshortreal") {
    return ConversionSysFnKind::kBitsToShortReal;
  }

  // Math functions -> MathSysFn
  if (name == "$ln") {
    return MathSysFn{MathFn::kLn};
  }
  if (name == "$log10") {
    return MathSysFn{MathFn::kLog10};
  }
  if (name == "$exp") {
    return MathSysFn{MathFn::kExp};
  }
  if (name == "$sqrt") {
    return MathSysFn{MathFn::kSqrt};
  }
  if (name == "$floor") {
    return MathSysFn{MathFn::kFloor};
  }
  if (name == "$ceil") {
    return MathSysFn{MathFn::kCeil};
  }
  if (name == "$sin") {
    return MathSysFn{MathFn::kSin};
  }
  if (name == "$cos") {
    return MathSysFn{MathFn::kCos};
  }
  if (name == "$tan") {
    return MathSysFn{MathFn::kTan};
  }
  if (name == "$asin") {
    return MathSysFn{MathFn::kAsin};
  }
  if (name == "$acos") {
    return MathSysFn{MathFn::kAcos};
  }
  if (name == "$atan") {
    return MathSysFn{MathFn::kAtan};
  }
  if (name == "$sinh") {
    return MathSysFn{MathFn::kSinh};
  }
  if (name == "$cosh") {
    return MathSysFn{MathFn::kCosh};
  }
  if (name == "$tanh") {
    return MathSysFn{MathFn::kTanh};
  }
  if (name == "$asinh") {
    return MathSysFn{MathFn::kAsinh};
  }
  if (name == "$acosh") {
    return MathSysFn{MathFn::kAcosh};
  }
  if (name == "$atanh") {
    return MathSysFn{MathFn::kAtanh};
  }
  if (name == "$clog2") {
    return MathSysFn{MathFn::kClog2};
  }
  if (name == "$pow") {
    return MathSysFn{MathFn::kPow};
  }
  if (name == "$atan2") {
    return MathSysFn{MathFn::kAtan2};
  }
  if (name == "$hypot") {
    return MathSysFn{MathFn::kHypot};
  }

  // Timescale query functions -> constant
  if (name == "$timeunit") {
    return TimeScaleSysFnKind::kTimeunit;
  }
  if (name == "$timeprecision") {
    return TimeScaleSysFnKind::kTimeprecision;
  }

  // Type query functions -> constant
  if (name == "$bits") {
    return TypeQuerySysFnKind::kBits;
  }
  if (name == "$isunbounded") {
    return TypeQuerySysFnKind::kIsUnbounded;
  }
  if (name == "$typename") {
    return TypeQuerySysFnKind::kTypename;
  }

  // Array query functions (IEEE 1800-2023 20.7)
  if (name == "$left") {
    return ArrayQuerySysFnKind::kLeft;
  }
  if (name == "$right") {
    return ArrayQuerySysFnKind::kRight;
  }
  if (name == "$low") {
    return ArrayQuerySysFnKind::kLow;
  }
  if (name == "$high") {
    return ArrayQuerySysFnKind::kHigh;
  }
  if (name == "$increment") {
    return ArrayQuerySysFnKind::kIncrement;
  }
  if (name == "$size") {
    return ArrayQuerySysFnKind::kSize;
  }
  if (name == "$dimensions") {
    return ArrayQuerySysFnKind::kDimensions;
  }
  if (name == "$unpacked_dimensions") {
    return ArrayQuerySysFnKind::kUnpackedDimensions;
  }

  return std::nullopt;
}

namespace {

// Create kCast expression node
auto MakeCast(
    hir::ExpressionId operand, TypeId target_type, SourceSpan span,
    Context* ctx) -> hir::ExpressionId {
  return ctx->hir_arena->AddExpression(
      hir::Expression{
          .kind = hir::ExpressionKind::kCast,
          .type = target_type,
          .span = span,
          .data = hir::CastExpressionData{.operand = operand}});
}

// Create kBitCast expression node with validation
auto MakeBitCast(
    hir::ExpressionId operand, TypeId target_type, SourceSpan span,
    Context* ctx) -> hir::ExpressionId {
  const hir::Expression& operand_expr = (*ctx->hir_arena)[operand];
  TypeId source_type = operand_expr.type;

  const Type& src = (*ctx->type_arena)[source_type];
  const Type& dst = (*ctx->type_arena)[target_type];

  auto err = semantic::ValidateBitCast(src, dst, *ctx->type_arena);
  if (err) {
    ctx->sink->Error(span, *err);
    return hir::kInvalidExpressionId;
  }

  return ctx->hir_arena->AddExpression(
      hir::Expression{
          .kind = hir::ExpressionKind::kBitCast,
          .type = target_type,
          .span = span,
          .data = hir::BitCastExpressionData{.operand = operand}});
}

auto LowerConversion(
    const slang::ast::CallExpression& call, ConversionSysFnKind kind,
    hir::ExpressionId operand, SourceSpan span, Context* ctx)
    -> hir::ExpressionId {
  const hir::Expression& operand_expr = (*ctx->hir_arena)[operand];

  switch (kind) {
    case ConversionSysFnKind::kSigned: {
      auto result =
          semantic::MakeSignedVariant(operand_expr.type, *ctx->type_arena);
      if (!result) {
        ctx->sink->Error(span, result.error());
        return hir::kInvalidExpressionId;
      }
      return MakeCast(operand, *result, span, ctx);
    }

    case ConversionSysFnKind::kUnsigned: {
      auto result =
          semantic::MakeUnsignedVariant(operand_expr.type, *ctx->type_arena);
      if (!result) {
        ctx->sink->Error(span, result.error());
        return hir::kInvalidExpressionId;
      }
      return MakeCast(operand, *result, span, ctx);
    }

    case ConversionSysFnKind::kItor: {
      TypeId real_type =
          ctx->type_arena->Intern(TypeKind::kReal, std::monostate{});
      return MakeCast(operand, real_type, span, ctx);
    }

    case ConversionSysFnKind::kRtoi: {
      TypeId integer_type = ctx->type_arena->Intern(
          TypeKind::kIntegral,
          IntegralInfo{
              .bit_width = 32, .is_signed = true, .is_four_state = true});
      return MakeCast(operand, integer_type, span, ctx);
    }

    case ConversionSysFnKind::kRealToBits: {
      TypeId target = semantic::GetBitVectorType(*ctx->type_arena, 64);
      return MakeBitCast(operand, target, span, ctx);
    }

    case ConversionSysFnKind::kBitsToReal: {
      TypeId real_type =
          ctx->type_arena->Intern(TypeKind::kReal, std::monostate{});
      return MakeBitCast(operand, real_type, span, ctx);
    }

    case ConversionSysFnKind::kShortRealToBits: {
      TypeId target = semantic::GetBitVectorType(*ctx->type_arena, 32);
      return MakeBitCast(operand, target, span, ctx);
    }

    case ConversionSysFnKind::kBitsToShortReal: {
      TypeId shortreal_type =
          ctx->type_arena->Intern(TypeKind::kShortReal, std::monostate{});
      return MakeBitCast(operand, shortreal_type, span, ctx);
    }
  }

  ctx->ErrorFmt(
      span, "unhandled conversion function: {}", call.getSubroutineName());
  return hir::kInvalidExpressionId;
}

auto MakeIntConstant(int32_t value, SourceSpan span, Context* ctx)
    -> hir::ExpressionId {
  TypeId int_type = ctx->type_arena->Intern(
      TypeKind::kIntegral,
      IntegralInfo{.bit_width = 32, .is_signed = true, .is_four_state = false});
  auto bits = static_cast<uint32_t>(value);
  IntegralConstant ic;
  ic.value = {static_cast<uint64_t>(bits)};
  ic.unknown = {0};
  ConstId cid = ctx->constant_arena->Intern(int_type, std::move(ic));
  return ctx->hir_arena->AddExpression(
      hir::Expression{
          .kind = hir::ExpressionKind::kConstant,
          .type = int_type,
          .span = span,
          .data = hir::ConstantExpressionData{.constant = cid}});
}

auto MakeBitConstant(uint8_t value, SourceSpan span, Context* ctx)
    -> hir::ExpressionId {
  TypeId bit_type = ctx->type_arena->Intern(
      TypeKind::kIntegral,
      IntegralInfo{.bit_width = 1, .is_signed = false, .is_four_state = false});
  IntegralConstant ic;
  ic.value = {static_cast<uint64_t>(value & 1)};
  ic.unknown = {0};
  ConstId cid = ctx->constant_arena->Intern(bit_type, std::move(ic));
  return ctx->hir_arena->AddExpression(
      hir::Expression{
          .kind = hir::ExpressionKind::kConstant,
          .type = bit_type,
          .span = span,
          .data = hir::ConstantExpressionData{.constant = cid}});
}

auto MakeStringConstant(std::string value, SourceSpan span, Context* ctx)
    -> hir::ExpressionId {
  TypeId string_type =
      ctx->type_arena->Intern(TypeKind::kString, std::monostate{});
  ConstId cid = ctx->constant_arena->Intern(
      string_type, StringConstant{.value = std::move(value)});
  return ctx->hir_arena->AddExpression(
      hir::Expression{
          .kind = hir::ExpressionKind::kConstant,
          .type = string_type,
          .span = span,
          .data = hir::ConstantExpressionData{.constant = cid}});
}

// Check if an expression is a "variable-rooted lvalue" per IEEE
// 1800-2023 20.7.1. Returns true if the expression is ultimately rooted at a
// named variable symbol, with only location-preserving operations (member
// selects, array index selects, part selects, bit selects, conversions).
//
// Accepted patterns (returns true):
//   - NamedValue (variable reference like 'arr')
//   - HierarchicalValue (hierarchical reference like 'mod.arr')
//   - ElementSelect of accepted (like 'arr[i]')
//   - RangeSelect of accepted (like 'arr[3:0]')
//   - MemberAccess of accepted (like 'struct.field')
//   - Implicit/Propagated Conversion of accepted
//
// Rejected patterns (returns false):
//   - Call (function return value)
//   - ConditionalOp (ternary result)
//   - BinaryOp / UnaryOp
//   - Concatenation / Replication
//   - NewArray
//   - Any other expression kind
auto IsArrayVariable(const slang::ast::Expression& expr) -> bool {
  using slang::ast::ExpressionKind;
  const slang::ast::Expression* current = &expr;

  while (true) {
    switch (current->kind) {
      case ExpressionKind::NamedValue:
      case ExpressionKind::HierarchicalValue:
        // Reached a variable root - this is an array variable
        return true;

      case ExpressionKind::ElementSelect: {
        const auto& sel = current->as<slang::ast::ElementSelectExpression>();
        current = &sel.value();
        continue;
      }

      case ExpressionKind::RangeSelect: {
        const auto& sel = current->as<slang::ast::RangeSelectExpression>();
        current = &sel.value();
        continue;
      }

      case ExpressionKind::MemberAccess: {
        const auto& mem = current->as<slang::ast::MemberAccessExpression>();
        current = &mem.value();
        continue;
      }

      case ExpressionKind::Conversion: {
        const auto& conv = current->as<slang::ast::ConversionExpression>();
        // Only implicit/propagated conversions preserve lvalue-ness
        using CK = slang::ast::ConversionKind;
        if (conv.conversionKind == CK::Implicit ||
            conv.conversionKind == CK::Propagated) {
          current = &conv.operand();
          continue;
        }
        // Explicit cast breaks variable-ness
        return false;
      }

      default:
        // Function calls, operators, concatenation, etc. are not variables
        return false;
    }
  }
}

// Create a 32-bit signed 'x (unknown) constant.
auto MakeIntX(SourceSpan span, Context* ctx) -> hir::ExpressionId {
  TypeId int_type = ctx->type_arena->Intern(
      TypeKind::kIntegral,
      IntegralInfo{.bit_width = 32, .is_signed = true, .is_four_state = true});
  IntegralConstant ic;
  ic.value = {0};
  ic.unknown = {0xFFFF'FFFFU};  // All bits unknown
  ConstId cid = ctx->constant_arena->Intern(int_type, std::move(ic));
  return ctx->hir_arena->AddExpression(
      hir::Expression{
          .kind = hir::ExpressionKind::kConstant,
          .type = int_type,
          .span = span,
          .data = hir::ConstantExpressionData{.constant = cid}});
}

// Compute $dimensions for a slang type.
// Returns total number of dimensions (packed + unpacked).
// Helper to count dimensions, tracking if we're inside a packed array.
// When inside_packed=true, scalar elements shouldn't be counted as dimensions.
auto ComputeDimensionsImpl(const slang::ast::Type& type, bool inside_packed)
    -> int32_t {
  // String is treated as having 1 dimension (IEEE 1800-2023 20.7)
  if (type.isString()) {
    return 1;
  }

  // Packed array contributes 1 dimension per level
  if (type.kind == slang::ast::SymbolKind::PackedArrayType) {
    const auto& packed = type.as<slang::ast::PackedArrayType>();
    return 1 + ComputeDimensionsImpl(packed.elementType, true);
  }

  // Unpacked array types
  if (type.kind == slang::ast::SymbolKind::FixedSizeUnpackedArrayType) {
    const auto& arr = type.as<slang::ast::FixedSizeUnpackedArrayType>();
    return 1 + ComputeDimensionsImpl(arr.elementType, false);
  }
  if (type.kind == slang::ast::SymbolKind::DynamicArrayType) {
    const auto& arr = type.as<slang::ast::DynamicArrayType>();
    return 1 + ComputeDimensionsImpl(arr.elementType, false);
  }
  if (type.kind == slang::ast::SymbolKind::QueueType) {
    const auto& q = type.as<slang::ast::QueueType>();
    return 1 + ComputeDimensionsImpl(q.elementType, false);
  }

  // Integral types at top-level are 1-dimensional (like packed[N-1:0])
  // But scalar types (logic, bit, reg) inside packed arrays are not separate
  // dimensions - they're the element type.
  if (type.isIntegral() && type.getBitWidth() > 0) {
    if (inside_packed && type.isScalar()) {
      return 0;  // Scalar element of packed array, not a dimension
    }
    return 1;
  }

  return 0;
}

// For string or simple bit-vector types, returns 1.
// For non-array types, returns 0.
auto ComputeDimensions(const slang::ast::Type& type) -> int32_t {
  return ComputeDimensionsImpl(type, false);
}

// Compute $unpacked_dimensions for a slang type.
// Returns the number of unpacked dimensions only.
auto ComputeUnpackedDimensions(const slang::ast::Type& type) -> int32_t {
  // Unpacked array types contribute to count
  if (type.kind == slang::ast::SymbolKind::FixedSizeUnpackedArrayType) {
    const auto& arr = type.as<slang::ast::FixedSizeUnpackedArrayType>();
    return 1 + ComputeUnpackedDimensions(arr.elementType);
  }
  if (type.kind == slang::ast::SymbolKind::DynamicArrayType) {
    const auto& arr = type.as<slang::ast::DynamicArrayType>();
    return 1 + ComputeUnpackedDimensions(arr.elementType);
  }
  if (type.kind == slang::ast::SymbolKind::QueueType) {
    const auto& q = type.as<slang::ast::QueueType>();
    return 1 + ComputeUnpackedDimensions(q.elementType);
  }

  // Packed types don't contribute to unpacked count
  return 0;
}

// Get the slang type for dimension N (1-indexed).
// Returns nullptr if dimension is out of range.
// Unpacked dimensions come first (slowest-varying), then packed.
auto GetDimensionType(const slang::ast::Type& type, int32_t dim)
    -> const slang::ast::Type* {
  if (dim < 1) {
    return nullptr;
  }

  // Count unpacked dimensions first
  int32_t unpacked_dims = ComputeUnpackedDimensions(type);

  if (dim <= unpacked_dims) {
    // Navigate through unpacked dimensions
    const slang::ast::Type* current = &type;
    for (int32_t i = 1; i < dim; ++i) {
      if (current->kind == slang::ast::SymbolKind::FixedSizeUnpackedArrayType) {
        current =
            &current->as<slang::ast::FixedSizeUnpackedArrayType>().elementType;
      } else if (current->kind == slang::ast::SymbolKind::DynamicArrayType) {
        current = &current->as<slang::ast::DynamicArrayType>().elementType;
      } else if (current->kind == slang::ast::SymbolKind::QueueType) {
        current = &current->as<slang::ast::QueueType>().elementType;
      } else {
        return nullptr;
      }
    }
    return current;
  }

  // Navigate to packed portion
  const slang::ast::Type* current = &type;
  for (int32_t i = 0; i < unpacked_dims; ++i) {
    if (current->kind == slang::ast::SymbolKind::FixedSizeUnpackedArrayType) {
      current =
          &current->as<slang::ast::FixedSizeUnpackedArrayType>().elementType;
    } else if (current->kind == slang::ast::SymbolKind::DynamicArrayType) {
      current = &current->as<slang::ast::DynamicArrayType>().elementType;
    } else if (current->kind == slang::ast::SymbolKind::QueueType) {
      current = &current->as<slang::ast::QueueType>().elementType;
    } else {
      return nullptr;
    }
  }

  // Now navigate packed dimensions
  int32_t packed_dim = dim - unpacked_dims;
  for (int32_t i = 1; i < packed_dim; ++i) {
    if (current->kind == slang::ast::SymbolKind::PackedArrayType) {
      current = &current->as<slang::ast::PackedArrayType>().elementType;
    } else {
      return nullptr;
    }
  }

  // Check if we're at a valid dimension
  if (current->kind == slang::ast::SymbolKind::PackedArrayType ||
      current->kind == slang::ast::SymbolKind::FixedSizeUnpackedArrayType ||
      current->kind == slang::ast::SymbolKind::DynamicArrayType ||
      current->kind == slang::ast::SymbolKind::QueueType ||
      (current->isIntegral() && current->getBitWidth() > 0) ||
      current->isString()) {
    return current;
  }

  return nullptr;
}

// Check if a dimension type is variable-sized (dynamic array or queue).
auto IsDimensionVariableSized(const slang::ast::Type& dim_type) -> bool {
  return dim_type.kind == slang::ast::SymbolKind::DynamicArrayType ||
         dim_type.kind == slang::ast::SymbolKind::QueueType;
}

// Get left bound for a dimension type. Returns nullopt if variable-sized.
auto GetDimensionLeft(const slang::ast::Type& dim_type)
    -> std::optional<int32_t> {
  if (dim_type.kind == slang::ast::SymbolKind::PackedArrayType) {
    const auto& packed = dim_type.as<slang::ast::PackedArrayType>();
    return packed.range.left;
  }
  if (dim_type.kind == slang::ast::SymbolKind::FixedSizeUnpackedArrayType) {
    const auto& arr = dim_type.as<slang::ast::FixedSizeUnpackedArrayType>();
    return arr.range.left;
  }
  if (dim_type.kind == slang::ast::SymbolKind::DynamicArrayType ||
      dim_type.kind == slang::ast::SymbolKind::QueueType) {
    return 0;  // Variable-sized: left is always 0
  }
  // Integral type: implicit [bit_width-1:0]
  if (dim_type.isIntegral()) {
    return static_cast<int32_t>(dim_type.getBitWidth()) - 1;
  }
  // String: dimension functions are NOT defined by LRM (only $dimensions=1)
  // Return nullopt to trigger 'x result
  return std::nullopt;
}

// Get right bound for a dimension type. Returns nullopt if needs runtime.
auto GetDimensionRight(const slang::ast::Type& dim_type)
    -> std::optional<int32_t> {
  if (dim_type.kind == slang::ast::SymbolKind::PackedArrayType) {
    const auto& packed = dim_type.as<slang::ast::PackedArrayType>();
    return packed.range.right;
  }
  if (dim_type.kind == slang::ast::SymbolKind::FixedSizeUnpackedArrayType) {
    const auto& arr = dim_type.as<slang::ast::FixedSizeUnpackedArrayType>();
    return arr.range.right;
  }
  // Dynamic/queue: right needs runtime size
  if (dim_type.kind == slang::ast::SymbolKind::DynamicArrayType ||
      dim_type.kind == slang::ast::SymbolKind::QueueType) {
    return std::nullopt;  // Needs runtime
  }
  // String: dimension functions NOT defined by LRM - fall through to nullopt
  // Integral type: implicit [bit_width-1:0]
  if (dim_type.isIntegral()) {
    return 0;
  }
  return std::nullopt;
}

// Compute $increment for a fixed dimension type.
// Returns 1 if left >= right (descending), -1 if left < right (ascending).
// Variable-sized dimensions always return -1.
auto ComputeIncrement(const slang::ast::Type& dim_type) -> int32_t {
  auto left = GetDimensionLeft(dim_type);
  auto right = GetDimensionRight(dim_type);
  if (!left || !right) {
    return -1;  // Variable-sized
  }
  return (*left >= *right) ? 1 : -1;
}

// Check if a dimension function can be constant-folded for the given dimension.
// Returns true if dim is constant and the target dimension is fixed-size.
auto CanFoldDimensionQuery(
    ArrayQuerySysFnKind kind, const slang::ast::Type& array_type, int32_t dim)
    -> bool {
  int32_t total_dims = ComputeDimensions(array_type);
  if (dim < 1 || dim > total_dims) {
    return true;  // Out of range -> fold to 'x
  }

  const slang::ast::Type* dim_type = GetDimensionType(array_type, dim);
  if (dim_type == nullptr) {
    return true;  // Invalid -> fold to 'x
  }

  // String: dimension functions are NOT defined by LRM (only $dimensions=1).
  // Always fold to 'x.
  if (dim_type->isString()) {
    return true;
  }

  // $left and $increment can always be folded (even for variable-sized)
  if (kind == ArrayQuerySysFnKind::kLeft ||
      kind == ArrayQuerySysFnKind::kIncrement) {
    return true;
  }

  // For variable-sized dimensions, $right/$size/$low/$high need runtime
  if (IsDimensionVariableSized(*dim_type)) {
    return false;
  }

  return true;
}

// Fold an array query function to a constant.
// Precondition: CanFoldDimensionQuery returned true, or this is
// $dimensions/$unpacked_dimensions.
auto FoldArrayQuery(
    ArrayQuerySysFnKind kind, const slang::ast::Type& array_type, int32_t dim,
    SourceSpan span, Context* ctx) -> hir::ExpressionId {
  // $dimensions and $unpacked_dimensions are always foldable
  if (kind == ArrayQuerySysFnKind::kDimensions) {
    return MakeIntConstant(ComputeDimensions(array_type), span, ctx);
  }
  if (kind == ArrayQuerySysFnKind::kUnpackedDimensions) {
    return MakeIntConstant(ComputeUnpackedDimensions(array_type), span, ctx);
  }

  // Out-of-range dimension -> return 'x
  int32_t total_dims = ComputeDimensions(array_type);
  if (dim < 1 || dim > total_dims) {
    return MakeIntX(span, ctx);
  }

  const slang::ast::Type* dim_type = GetDimensionType(array_type, dim);
  if (dim_type == nullptr) {
    return MakeIntX(span, ctx);
  }

  // String: dimension functions are NOT defined by LRM (only $dimensions=1).
  // Return 'x for all dimension functions on string.
  if (dim_type->isString()) {
    return MakeIntX(span, ctx);
  }

  // Compute query result
  auto left_opt = GetDimensionLeft(*dim_type);
  auto right_opt = GetDimensionRight(*dim_type);
  int32_t increment = ComputeIncrement(*dim_type);

  // $left is always known (even for variable-sized: returns 0)
  if (kind == ArrayQuerySysFnKind::kLeft) {
    return MakeIntConstant(left_opt.value_or(0), span, ctx);
  }

  // $increment is always known
  if (kind == ArrayQuerySysFnKind::kIncrement) {
    return MakeIntConstant(increment, span, ctx);
  }

  // $low for fixed-size: (increment == -1) ? left : right
  if (kind == ArrayQuerySysFnKind::kLow) {
    if (!left_opt || !right_opt) {
      return MakeIntX(span, ctx);  // Should not happen for foldable
    }
    int32_t low = (increment == -1) ? *left_opt : *right_opt;
    return MakeIntConstant(low, span, ctx);
  }

  // $high for fixed-size: (increment == -1) ? right : left
  if (kind == ArrayQuerySysFnKind::kHigh) {
    if (!left_opt || !right_opt) {
      return MakeIntX(span, ctx);
    }
    int32_t high = (increment == -1) ? *right_opt : *left_opt;
    return MakeIntConstant(high, span, ctx);
  }

  // $right needs runtime for variable-sized
  if (kind == ArrayQuerySysFnKind::kRight) {
    if (!right_opt) {
      return MakeIntX(span, ctx);  // Should not reach here if foldable
    }
    return MakeIntConstant(*right_opt, span, ctx);
  }

  // $size = high - low + 1
  if (kind == ArrayQuerySysFnKind::kSize) {
    if (!left_opt || !right_opt) {
      return MakeIntX(span, ctx);
    }
    int32_t low = (increment == -1) ? *left_opt : *right_opt;
    int32_t high = (increment == -1) ? *right_opt : *left_opt;
    int32_t size = high - low + 1;
    return MakeIntConstant(size, span, ctx);
  }

  return MakeIntX(span, ctx);
}

}  // namespace

auto LowerDesugarableSystemFunction(
    const slang::ast::CallExpression& call,
    const DesugarableClassification& classification,
    ExpressionLoweringView view) -> hir::ExpressionId {
  auto* ctx = view.context;
  SourceSpan span = ctx->SpanOf(call.sourceRange);

  return std::visit(
      common::Overloaded{
          [&](ConversionSysFnKind kind) -> hir::ExpressionId {
            if (call.arguments().size() != 1) {
              ctx->ErrorFmt(
                  span, "{}() requires exactly one argument",
                  call.getSubroutineName());
              return hir::kInvalidExpressionId;
            }
            hir::ExpressionId operand =
                LowerExpression(*call.arguments()[0], view);
            if (!operand) {
              return hir::kInvalidExpressionId;
            }
            return LowerConversion(call, kind, operand, span, ctx);
          },
          [&](UnaryOpSysFn fn) -> hir::ExpressionId {
            if (call.arguments().size() != 1) {
              ctx->ErrorFmt(
                  span, "{}() requires exactly one argument",
                  call.getSubroutineName());
              return hir::kInvalidExpressionId;
            }
            hir::ExpressionId operand =
                LowerExpression(*call.arguments()[0], view);
            if (!operand) {
              return hir::kInvalidExpressionId;
            }
            TypeId result_type = LowerType(*call.type, span, ctx);
            return ctx->hir_arena->AddExpression(
                hir::Expression{
                    .kind = hir::ExpressionKind::kUnaryOp,
                    .type = result_type,
                    .span = span,
                    .data = hir::UnaryExpressionData{
                        .op = fn.op, .operand = operand}});
          },
          [&](BinaryOpSysFn fn) -> hir::ExpressionId {
            if (call.arguments().size() != 2) {
              ctx->ErrorFmt(
                  span, "{}() requires exactly two arguments",
                  call.getSubroutineName());
              return hir::kInvalidExpressionId;
            }
            hir::ExpressionId lhs = LowerExpression(*call.arguments()[0], view);
            if (!lhs) {
              return hir::kInvalidExpressionId;
            }
            hir::ExpressionId rhs = LowerExpression(*call.arguments()[1], view);
            if (!rhs) {
              return hir::kInvalidExpressionId;
            }
            TypeId result_type = LowerType(*call.type, span, ctx);
            return ctx->hir_arena->AddExpression(
                hir::Expression{
                    .kind = hir::ExpressionKind::kBinaryOp,
                    .type = result_type,
                    .span = span,
                    .data = hir::BinaryExpressionData{
                        .op = fn.op, .lhs = lhs, .rhs = rhs}});
          },
          [&](TimeScaleSysFnKind kind) -> hir::ExpressionId {
            using slang::ast::ArbitrarySymbolExpression;
            using slang::ast::CompilationUnitSymbol;
            using slang::ast::InstanceSymbol;

            if (call.arguments().size() > 1) {
              ctx->ErrorFmt(
                  span, "{}() takes at most one argument",
                  call.getSubroutineName());
              return hir::kInvalidExpressionId;
            }

            const auto& sys_info =
                std::get<slang::ast::CallExpression::SystemCallInfo>(
                    call.subroutine);

            auto extract_power = [&](std::optional<slang::TimeScale> ts) {
              if (!ts) {
                return kDefaultTimeScalePower;
              }
              return TimeScaleValueToPower(
                  kind == TimeScaleSysFnKind::kTimeunit ? ts->base
                                                        : ts->precision);
            };

            int power = kDefaultTimeScalePower;
            if (call.arguments().empty()) {
              auto ts = sys_info.scope->getTimeScale();
              power = extract_power(ts);
            } else {
              const auto* arg_expr = call.arguments()[0];
              if (arg_expr->kind !=
                  slang::ast::ExpressionKind::ArbitrarySymbol) {
                ctx->ErrorFmt(
                    span,
                    "{}() argument must be a module/interface "
                    "instance, $root, or $unit",
                    call.getSubroutineName());
                return hir::kInvalidExpressionId;
              }
              const auto& arg = arg_expr->as<ArbitrarySymbolExpression>();
              const auto& sym = *arg.symbol;

              if (sym.kind == slang::ast::SymbolKind::Root) {
                power =
                    ComputeGlobalPrecision(sys_info.scope->getCompilation());
              } else if (sym.kind == slang::ast::SymbolKind::CompilationUnit) {
                auto ts = sym.as<CompilationUnitSymbol>().getTimeScale();
                power = extract_power(ts);
              } else if (sym.kind == slang::ast::SymbolKind::Instance) {
                auto ts = sym.as<InstanceSymbol>().body.getTimeScale();
                power = extract_power(ts);
              } else {
                ctx->ErrorFmt(
                    span, "{}() unexpected symbol kind in argument",
                    call.getSubroutineName());
                return hir::kInvalidExpressionId;
              }
            }

            return MakeIntConstant(power, span, ctx);
          },
          [&](MathSysFn fn) -> hir::ExpressionId {
            int expected_arity = GetMathFnArity(fn.fn);
            if (std::cmp_not_equal(call.arguments().size(), expected_arity)) {
              ctx->ErrorFmt(
                  span, "{}() requires exactly {} argument(s)",
                  call.getSubroutineName(), expected_arity);
              return hir::kInvalidExpressionId;
            }
            std::vector<hir::ExpressionId> args;
            args.reserve(static_cast<size_t>(expected_arity));
            for (int i = 0; i < expected_arity; ++i) {
              hir::ExpressionId arg =
                  LowerExpression(*call.arguments()[i], view);
              if (!arg) {
                return hir::kInvalidExpressionId;
              }
              args.push_back(arg);
            }
            TypeId result_type = LowerType(*call.type, span, ctx);
            return ctx->hir_arena->AddExpression(
                hir::Expression{
                    .kind = hir::ExpressionKind::kMathCall,
                    .type = result_type,
                    .span = span,
                    .data = hir::MathCallExpressionData{
                        .fn = fn.fn, .args = std::move(args)}});
          },
          [&](TypeQuerySysFnKind kind) -> hir::ExpressionId {
            if (call.arguments().size() != 1) {
              ctx->ErrorFmt(
                  span, "{}() requires exactly one argument",
                  call.getSubroutineName());
              return hir::kInvalidExpressionId;
            }

            const auto& arg = *call.arguments()[0];
            const slang::ast::Type& target_type = *arg.type;

            switch (kind) {
              case TypeQuerySysFnKind::kBits: {
                // Reject hierarchical references (same guard as slang's
                // noHierarchical)
                if (arg.hasHierarchicalReference()) {
                  ctx->ErrorFmt(
                      span, "$bits() does not support hierarchical references");
                  return hir::kInvalidExpressionId;
                }

                // Only support fixed-size types (no runtime evaluation)
                if (!target_type.isFixedSize()) {
                  ctx->ErrorFmt(span, "$bits() requires fixed-size type");
                  return hir::kInvalidExpressionId;
                }

                uint64_t width = target_type.getBitstreamWidth();
                // Intentional truncation to 32-bit signed int per IEEE 1800 /
                // slang behavior. Large widths wrap (defined overflow). See
                // slang QueryFuncs.cpp:69-72.
                return MakeIntConstant(static_cast<int32_t>(width), span, ctx);
              }

              case TypeQuerySysFnKind::kIsUnbounded: {
                bool is_unbounded = target_type.isUnbounded();

                // Also check parameter value (parameter may have bounded type
                // but $ value)
                if (!is_unbounded &&
                    arg.kind == slang::ast::ExpressionKind::NamedValue) {
                  const auto* sym = arg.getSymbolReference();
                  if (sym && sym->kind == slang::ast::SymbolKind::Parameter) {
                    const auto& param = sym->as<slang::ast::ParameterSymbol>();
                    if (param.getValue(arg.sourceRange).isUnbounded()) {
                      is_unbounded = true;
                    }
                  }
                }

                return MakeBitConstant(is_unbounded ? 1 : 0, span, ctx);
              }

              case TypeQuerySysFnKind::kTypename: {
                // Reject hierarchical references
                if (arg.hasHierarchicalReference()) {
                  ctx->ErrorFmt(
                      span,
                      "$typename() does not support hierarchical references");
                  return hir::kInvalidExpressionId;
                }

                slang::ast::TypePrinter printer;
                printer.append(target_type);
                return MakeStringConstant(printer.toString(), span, ctx);
              }
            }

            ctx->ErrorFmt(span, "unhandled type query function");
            return hir::kInvalidExpressionId;
          },
          [&](ArrayQuerySysFnKind kind) -> hir::ExpressionId {
            // $dimensions and $unpacked_dimensions take exactly 1 argument
            // Other dimension functions take 1 or 2 arguments
            bool is_dimensions_fn =
                (kind == ArrayQuerySysFnKind::kDimensions ||
                 kind == ArrayQuerySysFnKind::kUnpackedDimensions);

            if (is_dimensions_fn && call.arguments().size() != 1) {
              ctx->ErrorFmt(
                  span, "{}() requires exactly one argument",
                  call.getSubroutineName());
              return hir::kInvalidExpressionId;
            }
            if (!is_dimensions_fn &&
                (call.arguments().empty() || call.arguments().size() > 2)) {
              ctx->ErrorFmt(
                  span, "{}() requires 1 or 2 arguments",
                  call.getSubroutineName());
              return hir::kInvalidExpressionId;
            }

            const auto& arg0 = *call.arguments()[0];
            const slang::ast::Type& array_type = *arg0.type;

            // $dimensions and $unpacked_dimensions always fold to constant
            if (is_dimensions_fn) {
              return FoldArrayQuery(kind, array_type, 1, span, ctx);
            }

            // Get dimension argument (default = 1)
            int32_t const_dim = 1;
            bool dim_is_constant = true;
            const slang::ast::Expression* dim_expr = nullptr;

            if (call.arguments().size() > 1) {
              dim_expr = call.arguments()[1];
              // Check if dimension is a constant expression
              const auto* dim_cv = dim_expr->getConstant();
              if (dim_cv && dim_cv->isInteger() && !dim_cv->hasUnknown()) {
                const_dim = static_cast<int32_t>(
                    dim_cv->integer().as<int32_t>().value_or(1));
              } else {
                dim_is_constant = false;
              }
            }

            // Check for type-form (DataTypeExpression or TypeReference)
            // Type-form means the argument is a type, not a variable/expression
            bool is_type_form =
                (arg0.kind == slang::ast::ExpressionKind::DataType) ||
                (arg0.kind == slang::ast::ExpressionKind::TypeReference);

            // Type-form with dynamic type identifier is an error (IEEE 20.7)
            // "It is an error to use these functions directly on a dynamically
            // sized type identifier."
            if (is_type_form) {
              if (array_type.kind == slang::ast::SymbolKind::DynamicArrayType) {
                ctx->ErrorFmt(
                    span,
                    "{}() cannot be used on dynamic array type identifier",
                    call.getSubroutineName());
                return hir::kInvalidExpressionId;
              }
              if (array_type.kind == slang::ast::SymbolKind::QueueType) {
                ctx->ErrorFmt(
                    span, "{}() cannot be used on queue type identifier",
                    call.getSubroutineName());
                return hir::kInvalidExpressionId;
              }
            }

            // Type-form with non-constant dim is an error
            if (is_type_form && !dim_is_constant) {
              ctx->ErrorFmt(
                  span, "{}() with type argument requires constant dimension",
                  call.getSubroutineName());
              return hir::kInvalidExpressionId;
            }

            // Type-form: always fold (no runtime array to query)
            if (is_type_form) {
              return FoldArrayQuery(kind, array_type, const_dim, span, ctx);
            }

            // Check for IEEE 20.7.1 error: dim > 1 on variable-sized dimension
            // for array variable. Only applies when arg0 is a variable-rooted
            // lvalue (not function calls, operators, conditionals, etc.).
            if (dim_is_constant && const_dim > 1 && IsArrayVariable(arg0)) {
              const slang::ast::Type* target_dim_type =
                  GetDimensionType(array_type, const_dim);
              if (target_dim_type &&
                  IsDimensionVariableSized(*target_dim_type)) {
                ctx->ErrorFmt(
                    span,
                    "array query with dimension > 1 on variable-sized "
                    "dimension is illegal (IEEE 1800-2023 20.7.1)");
                return hir::kInvalidExpressionId;
              }
            }

            // Check if we can constant-fold
            if (dim_is_constant &&
                CanFoldDimensionQuery(kind, array_type, const_dim)) {
              return FoldArrayQuery(kind, array_type, const_dim, span, ctx);
            }

            // Need runtime path: emit HIR SystemCall with ArrayQueryData
            hir::ExpressionId array_id = LowerExpression(arg0, view);
            if (!array_id) {
              return hir::kInvalidExpressionId;
            }

            hir::ExpressionId dim_id;
            if (dim_expr) {
              dim_id = LowerExpression(*dim_expr, view);
              if (!dim_id) {
                return hir::kInvalidExpressionId;
              }
            } else {
              // Default dimension = 1
              dim_id = MakeIntConstant(1, span, ctx);
            }

            TypeId lyra_array_type = LowerType(array_type, span, ctx);
            TypeId result_type = LowerType(*call.type, span, ctx);

            return ctx->hir_arena->AddExpression(
                hir::Expression{
                    .kind = hir::ExpressionKind::kSystemCall,
                    .type = result_type,
                    .span = span,
                    .data = hir::SystemCallExpressionData{hir::ArrayQueryData{
                        .kind = kind,
                        .array = array_id,
                        .dim = dim_id,
                        .array_type = lyra_array_type}}});
          }},
      classification);
}

}  // namespace lyra::lowering::ast_to_hir
