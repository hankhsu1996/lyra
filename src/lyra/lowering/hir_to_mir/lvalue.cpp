#include "lyra/lowering/hir_to_mir/lvalue.hpp"

#include <type_traits>
#include <variant>

#include "lyra/common/constant.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
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

// Emit X/Z check for index. Returns 2-state 1-bit bool.
auto EmitIndexIsKnown(
    mir::Operand index, TypeId index_type, MirBuilder& builder)
    -> mir::Operand {
  const Context& ctx = builder.GetContext();
  const Type& type = (*ctx.type_arena)[index_type];
  TypeId bool_type = ctx.GetBitType();

  if (type.Kind() == TypeKind::kIntegral && !type.AsIntegral().is_four_state) {
    // 2-state type: always known
    return mir::Operand::Const(MakeIntegralConst(1, bool_type));
  }
  // 4-state type: emit IsKnown check
  return builder.EmitUnary(mir::UnaryOp::kIsKnown, index, bool_type);
}

// Emit bounds and X/Z validity check for packed array index.
auto EmitPackedIndexValidity(
    mir::Operand index, TypeId index_type, const Type& array_type,
    MirBuilder& builder) -> mir::Operand {
  const Context& ctx = builder.GetContext();
  const auto& packed_info = array_type.AsPackedArray();
  const auto& range = packed_info.range;
  const Type& idx_type = (*ctx.type_arena)[index_type];
  bool is_signed =
      IsPacked(idx_type) && IsPackedSigned(idx_type, *ctx.type_arena);
  TypeId bool_type = ctx.GetBitType();

  // Emit bounds constants
  auto lower_const =
      mir::Operand::Const(MakeIntegralConst(range.Lower(), index_type));
  auto upper_const =
      mir::Operand::Const(MakeIntegralConst(range.Upper(), index_type));

  // Bounds check - use signed ops for signed indices
  mir::Operand ge_lower;
  mir::Operand le_upper;
  if (is_signed) {
    ge_lower = builder.EmitBinary(
        mir::BinaryOp::kGreaterThanEqualSigned, index, lower_const, bool_type);
    le_upper = builder.EmitBinary(
        mir::BinaryOp::kLessThanEqualSigned, index, upper_const, bool_type);
  } else {
    ge_lower = builder.EmitBinary(
        mir::BinaryOp::kGreaterThanEqual, index, lower_const, bool_type);
    le_upper = builder.EmitBinary(
        mir::BinaryOp::kLessThanEqual, index, upper_const, bool_type);
  }
  auto in_bounds = builder.EmitBinary(
      mir::BinaryOp::kLogicalAnd, ge_lower, le_upper, bool_type);

  // X/Z check
  auto is_known = EmitIndexIsKnown(index, index_type, builder);

  // valid = in_bounds && is_known
  return builder.EmitBinary(
      mir::BinaryOp::kLogicalAnd, in_bounds, is_known, bool_type);
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
    -> LvalueResult {
  Context& ctx = builder.GetContext();
  return LvalueResult{
      .place = ctx.LookupPlace(data.symbol),
      .validity = MakeAlwaysValid(builder),
  };
}

auto LowerElementAccessLvalue(
    const hir::ElementAccessExpressionData& data, MirBuilder& builder)
    -> LvalueResult {
  Context& ctx = builder.GetContext();

  LvalueResult base = LowerLvalue(data.base, builder);
  mir::Operand index_operand = LowerExpression(data.index, builder);

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

  mir::Projection proj{
      .info = mir::IndexProjection{.index = index_operand},
  };
  mir::PlaceId result_place =
      ctx.mir_arena->DerivePlace(base.place, std::move(proj));

  // Unpacked array access - validity inherited from base
  // (unpacked array bounds are checked at runtime separately)
  return LvalueResult{
      .place = result_place,
      .validity = base.validity,
  };
}

auto LowerMemberAccessLvalue(
    const hir::MemberAccessExpressionData& data, MirBuilder& builder)
    -> LvalueResult {
  Context& ctx = builder.GetContext();

  LvalueResult base = LowerLvalue(data.base, builder);

  const mir::Place& base_place = (*ctx.mir_arena)[base.place];
  TypeId base_type_id = mir::TypeOfPlace(*ctx.type_arena, base_place);
  const Type& base_type = (*ctx.type_arena)[base_type_id];
  if (base_type.Kind() != TypeKind::kUnpackedStruct) {
    throw common::InternalError(
        "LowerMemberAccessLvalue", "base is not an unpacked struct");
  }

  mir::Projection proj{
      .info = mir::FieldProjection{.field_index = data.field_index},
  };
  mir::PlaceId result_place =
      ctx.mir_arena->DerivePlace(base.place, std::move(proj));

  // Field access is always valid - validity inherited from base
  return LvalueResult{
      .place = result_place,
      .validity = base.validity,
  };
}

auto LowerPackedElementSelectLvalue(
    const hir::PackedElementSelectExpressionData& data,
    const hir::Expression& expr, MirBuilder& builder) -> LvalueResult {
  Context& ctx = builder.GetContext();

  LvalueResult base = LowerLvalue(data.base, builder);
  mir::Operand index_operand = LowerExpression(data.index, builder);

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
  mir::Projection proj{
      .info =
          mir::BitRangeProjection{
              .bit_offset = offset,
              .width = element_width,
              .element_type = expr.type,
          },
  };
  mir::PlaceId result_place =
      ctx.mir_arena->DerivePlace(base.place, std::move(proj));

  return LvalueResult{
      .place = result_place,
      .validity = total_validity,
  };
}

}  // namespace

auto LowerLvalue(hir::ExpressionId expr_id, MirBuilder& builder)
    -> LvalueResult {
  Context& ctx = builder.GetContext();
  const hir::Expression& expr = (*ctx.hir_arena)[expr_id];

  return std::visit(
      [&](const auto& data) -> LvalueResult {
        using T = std::decay_t<decltype(data)>;
        if constexpr (std::is_same_v<T, hir::NameRefExpressionData>) {
          return LowerNameRefLvalue(data, builder);
        } else if constexpr (std::is_same_v<
                                 T, hir::ElementAccessExpressionData>) {
          return LowerElementAccessLvalue(data, builder);
        } else if constexpr (std::is_same_v<
                                 T, hir::MemberAccessExpressionData>) {
          return LowerMemberAccessLvalue(data, builder);
        } else if constexpr (std::is_same_v<
                                 T, hir::PackedElementSelectExpressionData>) {
          return LowerPackedElementSelectLvalue(data, expr, builder);
        } else {
          throw common::InternalError(
              "LowerLvalue", "unsupported lvalue expression");
        }
      },
      expr.data);
}

}  // namespace lyra::lowering::hir_to_mir
