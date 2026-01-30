#include "lyra/lowering/hir_to_mir/lvalue.hpp"

#include <algorithm>
#include <climits>
#include <cstdint>
#include <expected>
#include <format>
#include <type_traits>
#include <utility>
#include <variant>

#include "lyra/common/constant.hpp"
#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/integral_constant.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/origin_id.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_queries.hpp"
#include "lyra/hir/expression.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/lowering/hir_to_mir/builder.hpp"
#include "lyra/lowering/hir_to_mir/context.hpp"
#include "lyra/lowering/hir_to_mir/expression.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/operator.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/place_type.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// Make an integral constant with specified value and type.
auto MakeIntegralConst(int64_t value, TypeId type) -> Constant {
  IntegralConstant ic;
  ic.value.push_back(static_cast<uint64_t>(value));
  ic.unknown.push_back(0);
  return Constant{.type = type, .value = std::move(ic)};
}

// Create a constant 1 validity (always valid).
auto MakeAlwaysValid(MirBuilder& builder) -> mir::Operand {
  TypeId bool_type = builder.GetContext().GetBitType();
  return mir::Operand::Const(MakeIntegralConst(1, bool_type));
}

// Check if validity is constant 1 (always valid).
auto IsAlwaysValid(const mir::Operand& validity) -> bool {
  if (validity.kind != mir::Operand::Kind::kConst) {
    return false;
  }
  const auto& constant = std::get<Constant>(validity.payload);
  const auto* ic = std::get_if<IntegralConstant>(&constant.value);
  if (ic == nullptr || ic->value.empty()) {
    return false;
  }
  return ic->value[0] == 1;
}

// Compose two validities with &&. Optimizes for constant 1 cases.
auto ComposeValidity(
    mir::Operand base_valid, mir::Operand our_valid, MirBuilder& builder)
    -> mir::Operand {
  if (IsAlwaysValid(base_valid)) {
    return our_valid;
  }
  if (IsAlwaysValid(our_valid)) {
    return base_valid;
  }
  TypeId bool_type = builder.GetContext().GetBitType();
  return builder.EmitBinary(
      mir::BinaryOp::kLogicalAnd, base_valid, our_valid, bool_type);
}

// Check if index type is 4-state (has X/Z bits).
auto IsFourStateIndex(TypeId index_type, const TypeArena& types) -> bool {
  const Type& type = types[index_type];
  if (IsPacked(type)) {
    return IsPackedFourState(type, types);
  }
  return false;
}

// Emit bounds and X/Z validity check for packed array index.
auto EmitPackedIndexValidity(
    mir::Operand index, TypeId index_type, const Type& array_type,
    MirBuilder& builder) -> mir::Operand {
  const Context& ctx = builder.GetContext();
  const auto& packed_info = array_type.AsPackedArray();
  const auto& range = packed_info.range;
  bool check_known = IsFourStateIndex(index_type, *ctx.type_arena);
  return builder.EmitIndexValidity(
      index, range.Lower(), range.Upper(), check_known);
}

// Emit offset computation for packed array element select.
// Uses 32-bit offset type to avoid truncation when index type is narrow.
auto EmitPackedElementOffset(
    mir::Operand index, TypeId /*index_type*/, const Type& array_type,
    MirBuilder& builder) -> mir::Operand {
  const Context& ctx = builder.GetContext();
  const auto& packed_info = array_type.AsPackedArray();
  const auto& range = packed_info.range;
  uint32_t element_width = PackedArrayElementWidth(array_type, *ctx.type_arena);
  TypeId offset_type = ctx.GetOffsetType();

  // Compute offset based on direction using 32-bit arithmetic
  mir::Operand offset;
  if (range.IsDescending()) {
    auto lower_const =
        mir::Operand::Const(MakeIntegralConst(range.Lower(), offset_type));
    auto diff = builder.EmitBinary(
        mir::BinaryOp::kSubtract, index, lower_const, offset_type);
    auto width_const =
        mir::Operand::Const(MakeIntegralConst(element_width, offset_type));
    offset = builder.EmitBinary(
        mir::BinaryOp::kMultiply, diff, width_const, offset_type);
  } else {
    auto upper_const =
        mir::Operand::Const(MakeIntegralConst(range.Upper(), offset_type));
    auto diff = builder.EmitBinary(
        mir::BinaryOp::kSubtract, upper_const, index, offset_type);
    auto width_const =
        mir::Operand::Const(MakeIntegralConst(element_width, offset_type));
    offset = builder.EmitBinary(
        mir::BinaryOp::kMultiply, diff, width_const, offset_type);
  }
  return offset;
}

auto LowerNameRefLvalue(
    const hir::NameRefExpressionData& data, MirBuilder& builder)
    -> Result<LvalueResult> {
  Context& ctx = builder.GetContext();
  return LvalueResult{
      .place = ctx.LookupPlace(data.symbol),
      .validity = MakeAlwaysValid(builder),
  };
}

auto LowerElementAccessLvalue(
    const hir::ElementAccessExpressionData& data, hir::ExpressionId expr_id,
    MirBuilder& builder) -> Result<LvalueResult> {
  Context& ctx = builder.GetContext();

  Result<LvalueResult> base_result = LowerLvalue(data.base, builder);
  if (!base_result) return std::unexpected(base_result.error());
  LvalueResult base = *base_result;
  auto index_result = LowerExpression(data.index, builder);
  if (!index_result) return std::unexpected(index_result.error());
  mir::Operand index_operand = std::move(*index_result);

  const mir::Place& base_place = (*ctx.mir_arena)[base.place];
  TypeId base_type_id = mir::TypeOfPlace(*ctx.type_arena, base_place);
  const Type& base_type = (*ctx.type_arena)[base_type_id];
  if (base_type.Kind() != TypeKind::kUnpackedArray &&
      base_type.Kind() != TypeKind::kDynamicArray &&
      base_type.Kind() != TypeKind::kQueue) {
    throw common::InternalError(
        "LowerElementAccessLvalue", "base is not an array or queue");
  }
  if (index_operand.kind != mir::Operand::Kind::kConst &&
      index_operand.kind != mir::Operand::Kind::kUse) {
    throw common::InternalError(
        "LowerElementAccessLvalue", "index operand must be Const or Use");
  }

  const hir::Expression& index_expr = (*ctx.hir_arena)[data.index];
  index_operand = NormalizeUnpackedIndex(
      index_operand, index_expr.type, base_type, builder);

  common::OriginId origin = builder.RecordProjectionOrigin(expr_id);
  mir::Projection proj{
      .info = mir::IndexProjection{.index = index_operand},
      .origin = origin,
  };
  mir::PlaceId result_place =
      ctx.mir_arena->DerivePlace(base.place, std::move(proj));

  // Unpacked/dynamic array and queue access - validity inherited from base.
  // TODO(hankhsu): Add OOB validity tracking for these array types.
  // Per IEEE 1800-2023, OOB read -> X/0, OOB write -> no-op (same as packed).
  return LvalueResult{
      .place = result_place,
      .validity = base.validity,
  };
}

auto LowerMemberAccessLvalue(
    const hir::MemberAccessExpressionData& data, hir::ExpressionId expr_id,
    MirBuilder& builder) -> Result<LvalueResult> {
  Context& ctx = builder.GetContext();

  Result<LvalueResult> base_result = LowerLvalue(data.base, builder);
  if (!base_result) return std::unexpected(base_result.error());
  LvalueResult base = *base_result;

  const mir::Place& base_place = (*ctx.mir_arena)[base.place];
  TypeId base_type_id = mir::TypeOfPlace(*ctx.type_arena, base_place);
  const Type& base_type = (*ctx.type_arena)[base_type_id];
  if (base_type.Kind() != TypeKind::kUnpackedStruct) {
    throw common::InternalError(
        "LowerMemberAccessLvalue", "base is not an unpacked struct");
  }

  common::OriginId origin = builder.RecordProjectionOrigin(expr_id);
  mir::Projection proj{
      .info = mir::FieldProjection{.field_index = data.field_index},
      .origin = origin,
  };
  mir::PlaceId result_place =
      ctx.mir_arena->DerivePlace(base.place, std::move(proj));

  // Field access is always valid - validity inherited from base
  return LvalueResult{
      .place = result_place,
      .validity = base.validity,
  };
}

auto LowerUnionMemberAccessLvalue(
    const hir::UnionMemberAccessExpressionData& data, hir::ExpressionId expr_id,
    MirBuilder& builder) -> Result<LvalueResult> {
  Context& ctx = builder.GetContext();

  Result<LvalueResult> base_result = LowerLvalue(data.base, builder);
  if (!base_result) return std::unexpected(base_result.error());
  LvalueResult base = *base_result;

  const mir::Place& base_place = (*ctx.mir_arena)[base.place];
  TypeId base_type_id = mir::TypeOfPlace(*ctx.type_arena, base_place);
  const Type& base_type = (*ctx.type_arena)[base_type_id];
  if (base_type.Kind() != TypeKind::kUnpackedUnion) {
    throw common::InternalError(
        "LowerUnionMemberAccessLvalue", "base is not an unpacked union");
  }

  common::OriginId origin = builder.RecordProjectionOrigin(expr_id);
  mir::Projection proj{
      .info =
          mir::UnionMemberProjection{
              .member_index = static_cast<uint32_t>(data.member_index)},
      .origin = origin,
  };
  mir::PlaceId result_place =
      ctx.mir_arena->DerivePlace(base.place, std::move(proj));

  // Union member access is always valid - validity inherited from base
  return LvalueResult{
      .place = result_place,
      .validity = base.validity,
  };
}

auto LowerPackedElementSelectLvalue(
    const hir::PackedElementSelectExpressionData& data,
    hir::ExpressionId expr_id, const hir::Expression& expr, MirBuilder& builder)
    -> Result<LvalueResult> {
  Context& ctx = builder.GetContext();

  Result<LvalueResult> base_result = LowerLvalue(data.base, builder);
  if (!base_result) return std::unexpected(base_result.error());
  LvalueResult base = *base_result;
  auto index_result = LowerExpression(data.index, builder);
  if (!index_result) return std::unexpected(index_result.error());
  mir::Operand index_operand = std::move(*index_result);

  // Get type info
  const hir::Expression& base_expr = (*ctx.hir_arena)[data.base];
  const hir::Expression& index_expr = (*ctx.hir_arena)[data.index];
  const Type& base_type = (*ctx.type_arena)[base_expr.type];
  uint32_t element_width = PackedArrayElementWidth(base_type, *ctx.type_arena);

  // Compute validity for this access
  mir::Operand our_validity = EmitPackedIndexValidity(
      index_operand, index_expr.type, base_type, builder);

  // Compose with base validity
  mir::Operand total_validity =
      ComposeValidity(base.validity, our_validity, builder);

  // Compute bit offset
  mir::Operand offset = EmitPackedElementOffset(
      index_operand, index_expr.type, base_type, builder);

  // Create BitRangeProjection
  common::OriginId origin = builder.RecordProjectionOrigin(expr_id);
  mir::Projection proj{
      .info =
          mir::BitRangeProjection{
              .bit_offset = offset,
              .width = element_width,
              .element_type = expr.type,
          },
      .origin = origin,
  };
  mir::PlaceId result_place =
      ctx.mir_arena->DerivePlace(base.place, std::move(proj));

  return LvalueResult{
      .place = result_place,
      .validity = total_validity,
  };
}

// Compute offset and validity for indexed part-select.
// For ascending (+:): select bits [index .. index + width - 1]
// For descending (-:): select bits [index - width + 1 .. index]
// Returns {offset, valid} pair.
auto EmitIndexedPartSelectOffsetAndValidity(
    mir::Operand index, TypeId index_type, const Type& base_type,
    uint32_t width, bool ascending, MirBuilder& builder)
    -> std::pair<mir::Operand, mir::Operand> {
  const Context& ctx = builder.GetContext();
  TypeId offset_type = ctx.GetOffsetType();

  // Determine base range bounds
  int32_t lower = 0;
  int32_t upper = 0;
  bool base_descending = true;  // Default for kIntegral

  if (base_type.Kind() == TypeKind::kIntegral) {
    const auto& info = base_type.AsIntegral();
    lower = 0;
    upper = static_cast<int32_t>(info.bit_width) - 1;
    base_descending = true;
  } else if (base_type.Kind() == TypeKind::kPackedArray) {
    const auto& packed = base_type.AsPackedArray();
    lower = packed.range.Lower();
    upper = packed.range.Upper();
    base_descending = packed.range.IsDescending();
  } else if (base_type.Kind() == TypeKind::kPackedStruct) {
    const auto& info = base_type.AsPackedStruct();
    lower = 0;
    upper = static_cast<int32_t>(info.total_bit_width) - 1;
    base_descending = true;
  } else {
    throw common::InternalError(
        "EmitIndexedPartSelectOffsetAndValidity",
        "base must be kIntegral, kPackedArray, or kPackedStruct");
  }

  // Compute effective bounds for validity check using int64_t to avoid
  // overflow:
  // +: (ascending): valid index range is [lower, upper - width + 1]
  // -: (descending): valid index range is [lower + width - 1, upper]
  int64_t eff_lower_64 =
      ascending ? lower : (static_cast<int64_t>(lower) + width - 1);
  int64_t eff_upper_64 =
      ascending ? (static_cast<int64_t>(upper) - width + 1) : upper;

  mir::Operand valid;
  if (eff_lower_64 > eff_upper_64 || eff_lower_64 < INT32_MIN ||
      eff_lower_64 > INT32_MAX || eff_upper_64 < INT32_MIN ||
      eff_upper_64 > INT32_MAX) {
    // Width exceeds base range or overflow - always invalid
    TypeId bit_type = ctx.GetBitType();
    valid = mir::Operand::Const(MakeIntegralConst(0, bit_type));
  } else {
    bool check_known = IsFourStateIndex(index_type, *ctx.type_arena);
    valid = builder.EmitIndexValidity(
        index, static_cast<int32_t>(eff_lower_64),
        static_cast<int32_t>(eff_upper_64), check_known);
  }

  // Compute physical bit offset based on base direction and part-select
  // direction Physical layout: bit at Lower() is at physical position 0 Use
  // int64_t for intermediate calculations to avoid overflow
  mir::Operand offset;
  if (base_descending) {
    if (ascending) {
      // offset = index - lower
      auto lower_const =
          mir::Operand::Const(MakeIntegralConst(lower, offset_type));
      offset = builder.EmitBinary(
          mir::BinaryOp::kSubtract, index, lower_const, offset_type);
    } else {
      // offset = index - width + 1 - lower = index - (lower + width - 1)
      int64_t adjust = static_cast<int64_t>(lower) + width - 1;
      auto adjust_const =
          mir::Operand::Const(MakeIntegralConst(adjust, offset_type));
      offset = builder.EmitBinary(
          mir::BinaryOp::kSubtract, index, adjust_const, offset_type);
    }
  } else {
    // Ascending base
    if (ascending) {
      // offset = upper - index - width + 1 = (upper - width + 1) - index
      int64_t adjust = static_cast<int64_t>(upper) - width + 1;
      auto adjust_const =
          mir::Operand::Const(MakeIntegralConst(adjust, offset_type));
      offset = builder.EmitBinary(
          mir::BinaryOp::kSubtract, adjust_const, index, offset_type);
    } else {
      // offset = upper - index
      auto upper_const =
          mir::Operand::Const(MakeIntegralConst(upper, offset_type));
      offset = builder.EmitBinary(
          mir::BinaryOp::kSubtract, upper_const, index, offset_type);
    }
  }

  return {offset, valid};
}

auto LowerIndexedPartSelectLvalue(
    const hir::IndexedPartSelectExpressionData& data, hir::ExpressionId expr_id,
    const hir::Expression& expr, MirBuilder& builder) -> Result<LvalueResult> {
  Context& ctx = builder.GetContext();

  // Lower base as lvalue (recursive)
  Result<LvalueResult> base_result = LowerLvalue(data.base, builder);
  if (!base_result) return std::unexpected(base_result.error());
  LvalueResult base = *base_result;

  // Lower index as expression
  auto index_result = LowerExpression(data.index, builder);
  if (!index_result) return std::unexpected(index_result.error());
  mir::Operand index_operand = std::move(*index_result);

  // Get type info
  const hir::Expression& base_expr = (*ctx.hir_arena)[data.base];
  const hir::Expression& index_expr = (*ctx.hir_arena)[data.index];
  const Type& base_type = (*ctx.type_arena)[base_expr.type];

  // Compute offset and validity
  auto [offset, our_validity] = EmitIndexedPartSelectOffsetAndValidity(
      index_operand, index_expr.type, base_type, data.width, data.ascending,
      builder);

  // Compose with base validity
  mir::Operand total_validity =
      ComposeValidity(base.validity, our_validity, builder);

  // Create BitRangeProjection
  common::OriginId origin = builder.RecordProjectionOrigin(expr_id);
  mir::Projection proj{
      .info =
          mir::BitRangeProjection{
              .bit_offset = offset,
              .width = data.width,
              .element_type = expr.type,
          },
      .origin = origin,
  };
  mir::PlaceId result_place =
      ctx.mir_arena->DerivePlace(base.place, std::move(proj));

  return LvalueResult{
      .place = result_place,
      .validity = total_validity,
  };
}

auto LowerPackedFieldAccessLvalue(
    const hir::PackedFieldAccessExpressionData& data, hir::ExpressionId expr_id,
    const hir::Expression& expr, MirBuilder& builder) -> Result<LvalueResult> {
  Context& ctx = builder.GetContext();

  Result<LvalueResult> base_result = LowerLvalue(data.base, builder);
  if (!base_result) return std::unexpected(base_result.error());
  LvalueResult base = *base_result;

  // Create constant offset operand
  TypeId offset_type = ctx.GetOffsetType();
  mir::Operand offset =
      mir::Operand::Const(MakeIntegralConst(data.bit_offset, offset_type));

  // Create BitRangeProjection
  common::OriginId origin = builder.RecordProjectionOrigin(expr_id);
  mir::Projection proj{
      .info =
          mir::BitRangeProjection{
              .bit_offset = offset,
              .width = data.bit_width,
              .element_type = expr.type,
          },
      .origin = origin,
  };
  mir::PlaceId result_place =
      ctx.mir_arena->DerivePlace(base.place, std::move(proj));

  // Packed field access with constant offset is always valid
  // Just inherit base validity
  return LvalueResult{
      .place = result_place,
      .validity = base.validity,
  };
}

auto LowerRangeSelectLvalue(
    const hir::RangeSelectExpressionData& data, hir::ExpressionId expr_id,
    const hir::Expression& expr, MirBuilder& builder) -> Result<LvalueResult> {
  Context& ctx = builder.GetContext();

  // Lower base as lvalue (recursive)
  Result<LvalueResult> base_result = LowerLvalue(data.base, builder);
  if (!base_result) return std::unexpected(base_result.error());
  LvalueResult base = *base_result;

  // Get base type info
  const hir::Expression& base_expr = (*ctx.hir_arena)[data.base];
  const Type& base_type = (*ctx.type_arena)[base_expr.type];

  // Compute width from select bounds
  int32_t select_lower = std::min(data.left, data.right);
  int32_t select_upper = std::max(data.left, data.right);
  auto width = static_cast<uint32_t>(select_upper - select_lower + 1);

  // Compute bit offset based on base type direction
  // Physical bit layout: bit at Lower() is at physical position 0
  int32_t bit_offset = 0;
  switch (base_type.Kind()) {
    case TypeKind::kIntegral:
    case TypeKind::kPackedStruct:
      // kIntegral and kPackedStruct have implicit descending [bit_width-1:0]
      bit_offset = select_lower;
      break;
    case TypeKind::kPackedArray: {
      const auto& packed = base_type.AsPackedArray();
      if (packed.range.IsDescending()) {
        bit_offset = select_lower - packed.range.Lower();
      } else {
        // Ascending: higher logical index = lower physical position
        bit_offset = packed.range.Upper() - select_upper;
      }
      break;
    }
    default:
      throw common::InternalError(
          "LowerRangeSelectLvalue",
          "non-packed base type should have been rejected at AST->HIR");
  }

  // Create constant offset operand
  TypeId offset_type = ctx.GetOffsetType();
  mir::Operand offset =
      mir::Operand::Const(MakeIntegralConst(bit_offset, offset_type));

  // Create BitRangeProjection
  common::OriginId origin = builder.RecordProjectionOrigin(expr_id);
  mir::Projection proj{
      .info =
          mir::BitRangeProjection{
              .bit_offset = offset,
              .width = width,
              .element_type = expr.type,
          },
      .origin = origin,
  };
  mir::PlaceId result_place =
      ctx.mir_arena->DerivePlace(base.place, std::move(proj));

  // Range select with constant bounds is always valid - inherit base validity
  return LvalueResult{
      .place = result_place,
      .validity = base.validity,
  };
}

}  // namespace

auto NormalizeUnpackedIndex(
    mir::Operand index_operand, TypeId index_type, const Type& base_type,
    MirBuilder& builder) -> mir::Operand {
  switch (base_type.Kind()) {
    case TypeKind::kDynamicArray:
    case TypeKind::kQueue:
      return index_operand;
    case TypeKind::kUnpackedArray:
      break;
    default:
      throw common::InternalError(
          "NormalizeUnpackedIndex", std::format(
                                        "unexpected base type kind: {}",
                                        static_cast<int>(base_type.Kind())));
  }

  const auto& range = base_type.AsUnpackedArray().range;
  TypeId offset_type = builder.GetContext().GetOffsetType();

  mir::Operand index_i32 =
      builder.EmitCast(index_operand, index_type, offset_type);

  if (range.IsDescending()) {
    auto upper_const =
        mir::Operand::Const(MakeIntegralConst(range.Upper(), offset_type));
    return builder.EmitBinary(
        mir::BinaryOp::kSubtract, upper_const, index_i32, offset_type);
  }

  int32_t lower = range.Lower();
  if (lower == 0) {
    return index_i32;
  }
  auto lower_const = mir::Operand::Const(MakeIntegralConst(lower, offset_type));
  return builder.EmitBinary(
      mir::BinaryOp::kSubtract, index_i32, lower_const, offset_type);
}

auto LowerLvalue(hir::ExpressionId expr_id, MirBuilder& builder)
    -> Result<LvalueResult> {
  Context& ctx = builder.GetContext();
  const hir::Expression& expr = (*ctx.hir_arena)[expr_id];

  return std::visit(
      [&](const auto& data) -> Result<LvalueResult> {
        using T = std::decay_t<decltype(data)>;
        if constexpr (std::is_same_v<T, hir::NameRefExpressionData>) {
          return LowerNameRefLvalue(data, builder);
        } else if constexpr (std::is_same_v<
                                 T, hir::ElementAccessExpressionData>) {
          return LowerElementAccessLvalue(data, expr_id, builder);
        } else if constexpr (std::is_same_v<
                                 T, hir::MemberAccessExpressionData>) {
          return LowerMemberAccessLvalue(data, expr_id, builder);
        } else if constexpr (std::is_same_v<
                                 T, hir::UnionMemberAccessExpressionData>) {
          return LowerUnionMemberAccessLvalue(data, expr_id, builder);
        } else if constexpr (std::is_same_v<
                                 T, hir::PackedElementSelectExpressionData>) {
          return LowerPackedElementSelectLvalue(data, expr_id, expr, builder);
        } else if constexpr (std::is_same_v<
                                 T, hir::IndexedPartSelectExpressionData>) {
          return LowerIndexedPartSelectLvalue(data, expr_id, expr, builder);
        } else if constexpr (std::is_same_v<
                                 T, hir::PackedFieldAccessExpressionData>) {
          return LowerPackedFieldAccessLvalue(data, expr_id, expr, builder);
        } else if constexpr (std::is_same_v<
                                 T, hir::RangeSelectExpressionData>) {
          return LowerRangeSelectLvalue(data, expr_id, expr, builder);
        } else if constexpr (std::is_same_v<
                                 T, hir::HierarchicalRefExpressionData>) {
          return LvalueResult{
              .place = builder.GetContext().LookupPlace(data.target),
              .validity = MakeAlwaysValid(builder),
          };
        } else {
          // Use direct span from HIR expression for the diagnostic
          return std::unexpected(
              Diagnostic::Unsupported(
                  expr.span, "unsupported lvalue expression",
                  UnsupportedCategory::kFeature));
        }
      },
      expr.data);
}

namespace {

// Pure lvalue lowering helper - recursively builds a Place without emission.
// Returns diagnostic at the point of detection when purity is violated.
auto LowerPureLvaluePlaceImpl(hir::ExpressionId expr_id, const Context& ctx)
    -> Result<mir::PlaceId> {
  const hir::Expression& expr = (*ctx.hir_arena)[expr_id];

  return std::visit(
      [&](const auto& data) -> Result<mir::PlaceId> {
        using T = std::decay_t<decltype(data)>;

        if constexpr (std::is_same_v<T, hir::NameRefExpressionData>) {
          // Pure: simple symbol lookup
          return ctx.LookupPlace(data.symbol);

        } else if constexpr (std::is_same_v<
                                 T, hir::HierarchicalRefExpressionData>) {
          // Pure: hierarchical symbol lookup
          return ctx.LookupPlace(data.target);

        } else if constexpr (std::is_same_v<
                                 T, hir::MemberAccessExpressionData>) {
          // Pure: struct field access (constant field index)
          auto base_result = LowerPureLvaluePlaceImpl(data.base, ctx);
          if (!base_result) return base_result;

          mir::Projection proj{
              .info = mir::FieldProjection{.field_index = data.field_index},
              .origin = common::OriginId::Invalid(),
          };
          return ctx.mir_arena->DerivePlace(*base_result, std::move(proj));

        } else if constexpr (std::is_same_v<
                                 T, hir::UnionMemberAccessExpressionData>) {
          // Pure: union member access (constant member index)
          auto base_result = LowerPureLvaluePlaceImpl(data.base, ctx);
          if (!base_result) return base_result;

          mir::Projection proj{
              .info =
                  mir::UnionMemberProjection{
                      .member_index = static_cast<uint32_t>(data.member_index)},
              .origin = common::OriginId::Invalid(),
          };
          return ctx.mir_arena->DerivePlace(*base_result, std::move(proj));

        } else if constexpr (std::is_same_v<
                                 T, hir::PackedFieldAccessExpressionData>) {
          // Pure: packed struct field access (constant bit offset)
          auto base_result = LowerPureLvaluePlaceImpl(data.base, ctx);
          if (!base_result) return base_result;

          TypeId offset_type = ctx.GetOffsetType();
          mir::Operand offset = mir::Operand::Const(
              MakeIntegralConst(data.bit_offset, offset_type));

          mir::Projection proj{
              .info =
                  mir::BitRangeProjection{
                      .bit_offset = offset,
                      .width = data.bit_width,
                      .element_type = expr.type,
                  },
              .origin = common::OriginId::Invalid(),
          };
          return ctx.mir_arena->DerivePlace(*base_result, std::move(proj));

        } else if constexpr (std::is_same_v<
                                 T, hir::ElementAccessExpressionData>) {
          // Element access - only pure if index is constant
          const hir::Expression& index_expr = (*ctx.hir_arena)[data.index];
          if (index_expr.kind != hir::ExpressionKind::kConstant) {
            return std::unexpected(
                Diagnostic::Unsupported(
                    index_expr.span,
                    "port connection target requires constant array index",
                    UnsupportedCategory::kFeature));
          }

          auto base_result = LowerPureLvaluePlaceImpl(data.base, ctx);
          if (!base_result) return base_result;

          // Get constant index value from constant arena
          const auto& const_data =
              std::get<hir::ConstantExpressionData>(index_expr.data);
          const Constant& constant = (*ctx.constant_arena)[const_data.constant];
          const auto* ic = std::get_if<IntegralConstant>(&constant.value);
          if (ic == nullptr || ic->value.empty()) {
            return std::unexpected(
                Diagnostic::Unsupported(
                    index_expr.span,
                    "port connection target requires integral constant index",
                    UnsupportedCategory::kFeature));
          }

          // Normalize index based on array range
          const mir::Place& base_place = (*ctx.mir_arena)[*base_result];
          TypeId base_type_id = mir::TypeOfPlace(*ctx.type_arena, base_place);
          const Type& base_type = (*ctx.type_arena)[base_type_id];

          auto raw_index = static_cast<int64_t>(ic->value[0]);
          int64_t normalized = raw_index;  // Default for dynamic array/queue

          if (base_type.Kind() == TypeKind::kUnpackedArray) {
            const auto& range = base_type.AsUnpackedArray().range;
            if (range.IsDescending()) {
              normalized = range.Upper() - raw_index;
            } else {
              normalized = raw_index - range.Lower();
            }
          }

          TypeId offset_type = ctx.GetOffsetType();
          mir::Operand index_operand =
              mir::Operand::Const(MakeIntegralConst(normalized, offset_type));

          mir::Projection proj{
              .info = mir::IndexProjection{.index = index_operand},
              .origin = common::OriginId::Invalid(),
          };
          return ctx.mir_arena->DerivePlace(*base_result, std::move(proj));

        } else if constexpr (std::is_same_v<
                                 T, hir::PackedElementSelectExpressionData>) {
          // Packed element select with dynamic index - not pure
          return std::unexpected(
              Diagnostic::Unsupported(
                  expr.span,
                  "port connection target does not support packed element "
                  "select "
                  "(dynamic bit index)",
                  UnsupportedCategory::kFeature));

        } else if constexpr (std::is_same_v<
                                 T, hir::IndexedPartSelectExpressionData>) {
          // Indexed part-select - not pure (requires runtime offset)
          return std::unexpected(
              Diagnostic::Unsupported(
                  expr.span,
                  "port connection target does not support indexed part-select",
                  UnsupportedCategory::kFeature));

        } else if constexpr (std::is_same_v<T, hir::BitSelectExpressionData>) {
          // Bit select - only pure if index is constant
          const hir::Expression& index_expr = (*ctx.hir_arena)[data.index];
          if (index_expr.kind != hir::ExpressionKind::kConstant) {
            return std::unexpected(
                Diagnostic::Unsupported(
                    index_expr.span,
                    "port connection target requires constant bit index",
                    UnsupportedCategory::kFeature));
          }
          // Fall through to unsupported - bit select lowering is complex
          return std::unexpected(
              Diagnostic::Unsupported(
                  expr.span,
                  "port connection target does not support bit select",
                  UnsupportedCategory::kFeature));

        } else if constexpr (std::is_same_v<
                                 T, hir::RangeSelectExpressionData>) {
          // Range select with constant bounds could be supported, but for now
          // reject
          return std::unexpected(
              Diagnostic::Unsupported(
                  expr.span,
                  "port connection target does not support range select",
                  UnsupportedCategory::kFeature));

        } else {
          return std::unexpected(
              Diagnostic::Unsupported(
                  expr.span,
                  "unsupported expression type for port connection target",
                  UnsupportedCategory::kFeature));
        }
      },
      expr.data);
}

}  // namespace

auto LowerPureLvaluePlace(hir::ExpressionId expr_id, const Context& ctx)
    -> Result<mir::PlaceId> {
  return LowerPureLvaluePlaceImpl(expr_id, ctx);
}

}  // namespace lyra::lowering::hir_to_mir
