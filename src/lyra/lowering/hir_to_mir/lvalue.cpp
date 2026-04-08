#include "lyra/lowering/hir_to_mir/lvalue.hpp"

#include <algorithm>
#include <climits>
#include <cstdint>
#include <expected>
#include <format>
#include <memory>
#include <tuple>
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
#include "lyra/lowering/hir_to_mir/packed_alignment.hpp"
#include "lyra/mir/assoc_op.hpp"
#include "lyra/mir/builtin.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/operator.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/place_type.hpp"
#include "lyra/mir/rvalue.hpp"

namespace lyra::lowering::hir_to_mir {

auto LvalueResult::GetLocalPlace() const -> mir::PlaceId {
  const auto* place = std::get_if<mir::PlaceId>(&dest);
  if (place == nullptr) {
    throw common::InternalError(
        "LvalueResult::GetLocalPlace",
        "dest is ExternalRefId, not PlaceId -- cannot use as local place");
  }
  return *place;
}

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

// Ensure validity operand is block-stable (kConst or kUse, never kUseTemp).
// UseTemps are block-local SSA values that become stale when blocks change.
// Callers of LowerLvalue may lower expressions that create blocks (e.g.,
// ternary), so validity must survive across block boundaries.
auto MaterializeValidity(mir::Operand validity, MirBuilder& builder)
    -> mir::Operand {
  if (validity.kind != mir::Operand::Kind::kUseTemp) return validity;
  TypeId bool_type = builder.GetContext().GetBitType();
  mir::PlaceId temp = builder.GetContext().AllocTemp(bool_type);
  builder.EmitAssign(temp, std::move(validity));
  return mir::Operand::Use(temp);
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

// Emit bounds and X/Z validity check for packed array index.
auto EmitPackedIndexValidity(
    mir::Operand index, TypeId index_type, const Type& array_type,
    MirBuilder& builder) -> mir::Operand {
  const auto& packed_info = array_type.AsPackedArray();
  const auto& range = packed_info.range;
  return builder.EmitIndexAccessValidity(
      index, index_type, range.Lower(), range.Upper());
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

}  // namespace

auto EmitUnpackedIndexValidity(
    mir::Operand index, TypeId index_type, mir::PlaceId base_place,
    TypeId base_type_id, MirBuilder& builder) -> mir::Operand {
  const Context& ctx = builder.GetContext();
  const Type& base_type = (*ctx.type_arena)[base_type_id];
  bool is_four_state = IsFourStateIndex(index_type, *ctx.type_arena);

  switch (base_type.Kind()) {
    case TypeKind::kUnpackedArray: {
      // Check original index against declared bounds [Lower, Upper].
      const auto& range = base_type.AsUnpackedArray().range;

      // Constant-fold: if index is a compile-time constant and in bounds,
      // skip runtime check. Constants cannot contain X/Z, so knownness
      // is irrelevant.
      if (index.kind == mir::Operand::Kind::kConst) {
        const auto& constant = std::get<Constant>(index.payload);
        const auto* ic = std::get_if<IntegralConstant>(&constant.value);
        if (ic != nullptr && !ic->value.empty() && ic->value.size() == 1) {
          uint64_t raw = ic->value[0];
          // Sign-extend based on index type if signed.
          const Type& idx_type = (*ctx.type_arena)[constant.type];
          uint32_t bw = PackedBitWidth(idx_type, *ctx.type_arena);
          auto idx_val = static_cast<int64_t>(raw);
          if (bw > 0 && bw < 64 && IsPacked(idx_type) &&
              IsPackedSigned(idx_type, *ctx.type_arena)) {
            uint64_t sign_bit = 1ULL << (bw - 1);
            if ((raw & sign_bit) != 0) {
              raw |= ~((1ULL << bw) - 1);
            }
            idx_val = static_cast<int64_t>(raw);
          }
          if (idx_val >= range.Lower() && idx_val <= range.Upper()) {
            return MakeAlwaysValid(builder);
          }
        }
      }

      return builder.EmitIndexAccessValidity(
          index, index_type, range.Lower(), range.Upper());
    }

    case TypeKind::kDynamicArray:
    case TypeKind::kQueue: {
      TypeId bit_type = ctx.GetBitType();
      TypeId offset_type = ctx.GetOffsetType();

      // Step 1: X/Z knownness check (4-state indices only)
      mir::Operand validity = MakeAlwaysValid(builder);
      if (is_four_state) {
        validity = builder.EmitIsKnown(index);
      }

      // Step 2: Cast index to 2-state offset type (collapses X/Z to 0)
      mir::Operand index_2s = builder.EmitCast(index, index_type, offset_type);

      // Step 3: Signed non-negative check
      auto zero_const = mir::Operand::Const(MakeIntegralConst(0, offset_type));
      mir::Operand nonneg = builder.EmitBinary(
          mir::BinaryOp::kGreaterThanEqualSigned, index_2s, zero_const,
          bit_type);

      // Step 4: Unsigned bounds check (index < size)
      mir::BuiltinMethod size_method = (base_type.Kind() == TypeKind::kQueue)
                                           ? mir::BuiltinMethod::kQueueSize
                                           : mir::BuiltinMethod::kArraySize;
      mir::Rvalue size_rvalue{
          .operands = {mir::Operand::Use(base_place)},
          .info =
              mir::BuiltinCallRvalueInfo{
                  .method = size_method,
                  .result_type = offset_type,
                  .receiver = std::nullopt,
                  .enum_type = std::nullopt},
      };
      mir::PlaceId size_place =
          builder.EmitPlaceTemp(offset_type, std::move(size_rvalue));
      mir::Operand size_op = mir::Operand::Use(size_place);

      mir::Operand in_bounds = builder.EmitBinary(
          mir::BinaryOp::kLessThan, index_2s, size_op, bit_type);

      // Step 5: Compose: valid = nonneg && in_bounds && (is_known if 4-state)
      mir::Operand result = builder.EmitBinary(
          mir::BinaryOp::kLogicalAnd, nonneg, in_bounds, bit_type);
      if (!hir_to_mir::IsAlwaysValid(validity)) {
        result = builder.EmitBinary(
            mir::BinaryOp::kLogicalAnd, result, validity, bit_type);
      }
      return result;
    }

    default:
      throw common::InternalError(
          "EmitUnpackedIndexValidity", std::format(
                                           "unexpected base type kind: {}",
                                           static_cast<int>(base_type.Kind())));
  }
}

namespace {

auto LowerNameRefLvalue(
    const hir::NameRefExpressionData& data, MirBuilder& builder)
    -> Result<LvalueResult> {
  Context& ctx = builder.GetContext();
  return LvalueResult{
      .dest = mir::WriteTarget{ctx.LookupPlace(data.symbol)},
      .validity = MakeAlwaysValid(builder),
  };
}

auto LowerElementAccessLvalue(
    const hir::ElementAccessExpressionData& data, hir::ExpressionId expr_id,
    MirBuilder& builder) -> Result<LvalueResult> {
  Context& ctx = builder.GetContext();

  Result<LvalueResult> base_result = LowerLvalue(data.base, builder);
  if (!base_result) return std::unexpected(base_result.error());
  LvalueResult base = std::move(*base_result);
  auto index_result = LowerExpression(data.index, builder);
  if (!index_result) return std::unexpected(index_result.error());
  mir::Operand index_operand = std::move(*index_result);

  mir::PlaceId base_place_id = base.GetLocalPlace();
  const mir::Place& base_place = (*ctx.mir_arena)[base_place_id];
  TypeId base_type_id = mir::TypeOfPlace(*ctx.type_arena, base_place);
  const Type& base_type = (*ctx.type_arena)[base_type_id];

  // Associative array: copy-in via AssocGet, attach writeback token.
  if (base_type.Kind() == TypeKind::kAssociativeArray) {
    const auto& aa_info = base_type.AsAssociativeArray();
    mir::PlaceId temp = ctx.AllocTemp(aa_info.element_type);
    builder.EmitAssocOp(
        mir::AssocOp{
            .receiver = base_place_id,
            .data = mir::AssocGet{.dest = temp, .key = index_operand}});
    return LvalueResult{
        .dest = mir::WriteTarget{temp},
        .validity = base.validity,
        .writeback = std::make_unique<AssocWriteBack>(AssocWriteBack{
            .aa_place = base_place_id,
            .key = index_operand,
            .temp_place = temp}),
    };
  }

  if (base_type.Kind() != TypeKind::kUnpackedArray &&
      base_type.Kind() != TypeKind::kDynamicArray &&
      base_type.Kind() != TypeKind::kQueue) {
    throw common::InternalError(
        "LowerElementAccessLvalue", "base is not an array or queue");
  }

  // Compute validity against original index (before normalization).
  const hir::Expression& index_expr = (*ctx.hir_arena)[data.index];
  mir::Operand our_validity = EmitUnpackedIndexValidity(
      index_operand, index_expr.type, base_place_id, base_type_id, builder);

  // Normalize index for addressing (after validity).
  index_operand = NormalizeUnpackedIndex(
      index_operand, index_expr.type, base_type, builder);

  common::OriginId origin = builder.RecordProjectionOrigin(expr_id);
  mir::Projection proj{
      .info = mir::IndexProjection{.index = index_operand},
      .origin = origin,
  };
  mir::PlaceId result_place = ctx.DerivePlace(base_place_id, std::move(proj));

  mir::Operand total_validity =
      ComposeValidity(base.validity, our_validity, builder);
  return LvalueResult{
      .dest = mir::WriteTarget{result_place},
      .validity = MaterializeValidity(total_validity, builder),
  };
}

auto LowerMemberAccessLvalue(
    const hir::MemberAccessExpressionData& data, hir::ExpressionId expr_id,
    MirBuilder& builder) -> Result<LvalueResult> {
  Context& ctx = builder.GetContext();

  Result<LvalueResult> base_result = LowerLvalue(data.base, builder);
  if (!base_result) return std::unexpected(base_result.error());
  LvalueResult base = std::move(*base_result);

  mir::PlaceId base_place_id = base.GetLocalPlace();
  const mir::Place& base_place = (*ctx.mir_arena)[base_place_id];
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
  mir::PlaceId result_place = ctx.DerivePlace(base_place_id, std::move(proj));

  // Field access is always valid - validity inherited from base.
  // Writeback propagated from base (for AA copy-in/copy-out).
  return LvalueResult{
      .dest = mir::WriteTarget{result_place},
      .validity = base.validity,
      .writeback = std::move(base.writeback),
  };
}

auto LowerUnionMemberAccessLvalue(
    const hir::UnionMemberAccessExpressionData& data, hir::ExpressionId expr_id,
    MirBuilder& builder) -> Result<LvalueResult> {
  Context& ctx = builder.GetContext();

  Result<LvalueResult> base_result = LowerLvalue(data.base, builder);
  if (!base_result) return std::unexpected(base_result.error());
  LvalueResult base = std::move(*base_result);

  mir::PlaceId base_place_id = base.GetLocalPlace();
  const mir::Place& base_place = (*ctx.mir_arena)[base_place_id];
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
  mir::PlaceId result_place = ctx.DerivePlace(base_place_id, std::move(proj));

  // Union member access is always valid - validity inherited from base
  return LvalueResult{
      .dest = mir::WriteTarget{result_place},
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
  LvalueResult base = std::move(*base_result);
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
              .guaranteed_alignment_bits = PowerOfTwoAlignment(element_width),
          },
      .origin = origin,
  };
  mir::PlaceId result_place =
      ctx.DerivePlace(base.GetLocalPlace(), std::move(proj));

  return LvalueResult{
      .dest = mir::WriteTarget{result_place},
      .validity = MaterializeValidity(total_validity, builder),
  };
}

// Compute offset, validity, and alignment for indexed part-select.
// For ascending (+:): select bits [index .. index + width - 1]
// For descending (-:): select bits [index - width + 1 .. index]
// Returns {offset, valid, alignment} tuple.
//
// Alignment is co-produced with the offset so there is one authority for
// the offset formula and its alignment guarantee.
auto EmitIndexedPartSelectOffsetAndValidity(
    mir::Operand index, TypeId index_type, const Type& base_type,
    uint32_t width, bool ascending, uint32_t index_alignment,
    MirBuilder& builder) -> std::tuple<mir::Operand, mir::Operand, uint32_t> {
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
    valid = builder.EmitIndexAccessValidity(
        index, index_type, static_cast<int32_t>(eff_lower_64),
        static_cast<int32_t>(eff_upper_64));
  }

  // Compute physical bit offset and its alignment based on base direction
  // and part-select direction. Physical layout: bit at Lower() is at
  // physical position 0.
  //
  // Alignment is computed from the subtracted constant and the index
  // alignment at the same site that constructs the offset, so there is
  // one authority for the offset formula and its alignment guarantee.
  mir::Operand offset;
  int64_t subtracted_constant = 0;
  if (base_descending) {
    if (ascending) {
      // offset = index - lower
      subtracted_constant = lower;
      auto lower_const =
          mir::Operand::Const(MakeIntegralConst(lower, offset_type));
      offset = builder.EmitBinary(
          mir::BinaryOp::kSubtract, index, lower_const, offset_type);
    } else {
      // offset = index - (lower + width - 1)
      subtracted_constant = static_cast<int64_t>(lower) + width - 1;
      auto adjust_const = mir::Operand::Const(
          MakeIntegralConst(subtracted_constant, offset_type));
      offset = builder.EmitBinary(
          mir::BinaryOp::kSubtract, index, adjust_const, offset_type);
    }
  } else {
    // Ascending base: offset = constant - index.
    // For ascending base, index_alignment carries through subtraction
    // the same way (alignment(c - a) = min(alignment(c), alignment(a))).
    if (ascending) {
      // offset = (upper - width + 1) - index
      subtracted_constant = static_cast<int64_t>(upper) - width + 1;
      auto adjust_const = mir::Operand::Const(
          MakeIntegralConst(subtracted_constant, offset_type));
      offset = builder.EmitBinary(
          mir::BinaryOp::kSubtract, adjust_const, index, offset_type);
    } else {
      // offset = upper - index
      subtracted_constant = upper;
      auto upper_const =
          mir::Operand::Const(MakeIntegralConst(upper, offset_type));
      offset = builder.EmitBinary(
          mir::BinaryOp::kSubtract, upper_const, index, offset_type);
    }
  }

  uint32_t alignment =
      ComputePartSelectOffsetAlignment(index_alignment, subtracted_constant);

  return {offset, valid, alignment};
}

auto LowerIndexedPartSelectLvalue(
    const hir::IndexedPartSelectExpressionData& data, hir::ExpressionId expr_id,
    const hir::Expression& expr, MirBuilder& builder) -> Result<LvalueResult> {
  Context& ctx = builder.GetContext();

  // Lower base as lvalue (recursive)
  Result<LvalueResult> base_result = LowerLvalue(data.base, builder);
  if (!base_result) return std::unexpected(base_result.error());
  LvalueResult base = std::move(*base_result);

  // Lower index as expression
  auto index_result = LowerExpression(data.index, builder);
  if (!index_result) return std::unexpected(index_result.error());
  mir::Operand index_operand = std::move(*index_result);

  // Get type info
  const hir::Expression& base_expr = (*ctx.hir_arena)[data.base];
  const hir::Expression& index_expr = (*ctx.hir_arena)[data.index];
  const Type& base_type = (*ctx.type_arena)[base_expr.type];

  // Compute index alignment from HIR expression tree.
  uint32_t index_alignment = GetHirExpressionAlignmentBits(
      *ctx.hir_arena, *ctx.active_constant_arena, data.index);

  // Compute offset, validity, and alignment together. The offset
  // construction site is the single authority for both the formula
  // and its alignment guarantee.
  auto [offset, our_validity, alignment] =
      EmitIndexedPartSelectOffsetAndValidity(
          index_operand, index_expr.type, base_type, data.width, data.ascending,
          index_alignment, builder);

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
              .guaranteed_alignment_bits = alignment,
          },
      .origin = origin,
  };
  mir::PlaceId result_place =
      ctx.DerivePlace(base.GetLocalPlace(), std::move(proj));

  return LvalueResult{
      .dest = mir::WriteTarget{result_place},
      .validity = MaterializeValidity(total_validity, builder),
  };
}

auto LowerPackedFieldAccessLvalue(
    const hir::PackedFieldAccessExpressionData& data, hir::ExpressionId expr_id,
    const hir::Expression& expr, MirBuilder& builder) -> Result<LvalueResult> {
  Context& ctx = builder.GetContext();

  Result<LvalueResult> base_result = LowerLvalue(data.base, builder);
  if (!base_result) return std::unexpected(base_result.error());
  LvalueResult base = std::move(*base_result);

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
      ctx.DerivePlace(base.GetLocalPlace(), std::move(proj));

  // Packed field access with constant offset is always valid
  // Just inherit base validity
  return LvalueResult{
      .dest = mir::WriteTarget{result_place},
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
  LvalueResult base = std::move(*base_result);

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
      ctx.DerivePlace(base.GetLocalPlace(), std::move(proj));

  // Range select with constant bounds is always valid - inherit base validity
  return LvalueResult{
      .dest = mir::WriteTarget{result_place},
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
          auto& ctx = builder.GetContext();
          if (ctx.external_refs == nullptr) {
            throw common::InternalError(
                "LowerLvalue/HierarchicalRef",
                "hierarchical body write requires external-ref context");
          }
          auto ref = ctx.LowerHierarchicalRefToExternalRef(
              data, expr.type, mir::ExternalAccessKind::kReadWrite);
          return LvalueResult{
              .dest = mir::WriteTarget{ref.ref_id},
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
          // Hierarchical non-local targets cannot be lowered as pure PlaceId.
          // They must go through ExternalRefId + late normalization.
          return std::unexpected(
              Diagnostic::Unsupported(
                  expr.span,
                  "hierarchical reference in pure-place context "
                  "(index-plan uses ExternalRefId transport instead)",
                  UnsupportedCategory::kFeature));

        } else if constexpr (std::is_same_v<
                                 T, hir::MemberAccessExpressionData>) {
          // Pure: struct field access (constant field index)
          auto base_result = LowerPureLvaluePlaceImpl(data.base, ctx);
          if (!base_result) return base_result;

          mir::Projection proj{
              .info = mir::FieldProjection{.field_index = data.field_index},
              .origin = common::OriginId::Invalid(),
          };
          return ctx.DerivePlace(*base_result, std::move(proj));

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
          return ctx.DerivePlace(*base_result, std::move(proj));

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
          return ctx.DerivePlace(*base_result, std::move(proj));

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
          const Constant& constant =
              (*ctx.active_constant_arena)[const_data.constant];
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
          return ctx.DerivePlace(*base_result, std::move(proj));

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
