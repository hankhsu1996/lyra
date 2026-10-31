#include "lyra/lowering/hir_to_mir/select_position.hpp"

#include <cstdint>
#include <optional>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/type_id.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_builders.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

constexpr PositionMap kIdentity{
    .origin = 0, .reversed = false, .step = 1, .from_right = false};

auto PositionType(const mir::CompilationUnit& unit) -> mir::TypeId {
  return mir::PositionType(unit.types);
}

auto Arithmetic(
    mir::CompilationUnit& unit, mir::Builder block, mir::BinaryOp op,
    mir::ExprId lhs, mir::ExprId rhs) -> mir::ExprId {
  const mir::TypeId type = PositionType(unit);
  return block.Add(mir::MakeBinary(unit, block.Body(), op, lhs, rhs, type));
}

}  // namespace

auto BuildOrdinalPosition(
    mir::CompilationUnit& unit, mir::Builder block, mir::ExprId ordinal)
    -> mir::ExprId {
  const mir::TypeId type = PositionType(unit);
  if (block.Get(ordinal).type == type) {
    return ordinal;
  }
  return block.Add(
      mir::MakeBuiltinCall(
          unit, block.Body(), support::BuiltinFn::kToPosition, std::nullopt,
          {ordinal}, type));
}

auto BuildPositionSum(
    mir::CompilationUnit& unit, mir::Builder block, mir::ExprId base,
    mir::ExprId offset) -> mir::ExprId {
  const mir::ExprId lhs = BuildOrdinalPosition(unit, block, base);
  const mir::ExprId rhs = BuildOrdinalPosition(unit, block, offset);
  return Arithmetic(unit, block, mir::BinaryOp::kAdd, lhs, rhs);
}

auto PositionMapOf(const UnitLowerer& unit_lowerer, hir::TypeId receiver)
    -> PositionMap {
  // One vector numbered `[width-1:0]` (LRM 7.2.1, 7.3.1), a bit per step.
  constexpr PositionMap kBitsFromZero{
      .origin = 0, .reversed = false, .step = 1, .from_right = true};
  const auto numbers_no_parts = []() -> PositionMap {
    throw InternalError(
        "PositionMapOf: a select reaching by position has a receiver that "
        "numbers its parts, and this one does not");
  };
  return unit_lowerer.Hir().types.Get(receiver).Visit(
      Overloaded{
          [&](const hir::PackedArrayType& packed) {
            const std::uint64_t element_width =
                unit_lowerer.Unit()
                    .types.Get(unit_lowerer.TranslateType(packed.element_type))
                    .Integral()
                    .bit_width;
            return PositionMap{
                .origin = packed.dim.right,
                .reversed = packed.dim.IsAscending(),
                .step = static_cast<std::int64_t>(element_width),
                .from_right = true};
          },
          // An enumeration is numbered the way its base is (LRM 6.19).
          [&](const hir::EnumType& enumeration) {
            return PositionMapOf(
                unit_lowerer,
                unit_lowerer.Hir().DefinitionOf(enumeration).base_type);
          },
          [&](const hir::ScalarBitType&) { return kBitsFromZero; },
          [&](const hir::PackedStructType&) { return kBitsFromZero; },
          [&](const hir::PackedUnionType&) { return kBitsFromZero; },
          // Element order runs left to right (LRM 7.6), so a descending range
          // counts down from its left bound.
          [&](const hir::UnpackedArrayType& array) {
            return PositionMap{
                .origin = array.dim.left,
                .reversed = array.dim.left > array.dim.right,
                .step = 1,
                .from_right = false};
          },
          [&](const hir::DynamicArrayType&) { return kIdentity; },
          [&](const hir::QueueType&) { return kIdentity; },
          [&](const hir::StringType&) { return kIdentity; },
          [&](const hir::UnpackedStructType&) { return numbers_no_parts(); },
          [&](const hir::UnpackedUnionType&) { return numbers_no_parts(); },
          [&](const hir::AssociativeArrayType&) { return numbers_no_parts(); },
          [&](const hir::WildcardIndexType&) { return numbers_no_parts(); },
          [&](const hir::EventType&) { return numbers_no_parts(); },
          [&](const hir::RealType&) { return numbers_no_parts(); },
          [&](const hir::ShortRealType&) { return numbers_no_parts(); },
          [&](const hir::RealTimeType&) { return numbers_no_parts(); },
          [&](const hir::ChandleType&) { return numbers_no_parts(); },
          [&](const hir::ClassHandleType&) { return numbers_no_parts(); },
          [&](const hir::ImportedClassHandleType&) {
            return numbers_no_parts();
          },
          [&](const hir::WeakHandleType&) { return numbers_no_parts(); },
          [&](const hir::CovergroupHandleType&) { return numbers_no_parts(); },
          [&](const hir::UnitObjectType&) { return numbers_no_parts(); },
          [&](const hir::UnitObjectsType&) { return numbers_no_parts(); },
          [&](const hir::VirtualInterfaceType&) { return numbers_no_parts(); },
          [&](const hir::NullType&) { return numbers_no_parts(); },
          [&](const hir::DynamicType&) { return numbers_no_parts(); },
          [&](const hir::VoidType&) { return numbers_no_parts(); },
      });
}

auto BuildConstantPosition(
    mir::CompilationUnit& unit, mir::Builder block, std::int64_t position)
    -> mir::ExprId {
  return mir::BuildIntegralLiteral(
      unit, block, PositionType(unit),
      mir::IntegralConstant{
          .value_words = {static_cast<std::uint64_t>(position)},
          .state_words = {0}});
}

auto LeastSignificantBitType(
    const mir::CompilationUnit& unit, mir::TypeId value_type)
    -> std::optional<mir::TypeId> {
  const mir::Type& value = unit.types.Get(value_type);
  if (!value.IsIntegral()) {
    return std::nullopt;
  }
  return mir::PackedVectorOf(unit.types, 1, value.Integral().state_kind);
}

auto BuildLeastSignificantBit(
    const mir::CompilationUnit& unit, mir::Builder block, mir::ExprId value,
    mir::TypeId bit_type) -> mir::ExprId {
  const mir::ExprId lsb = mir::BuildIntegralLiteral(
      unit, block, PositionType(unit),
      mir::IntegralConstant{.value_words = {0}, .state_words = {0}});
  return block.Add(
      mir::MakeBuiltinCall(
          unit, block.Body(), support::BuiltinFn::kSlice, value, {lsb},
          bit_type));
}

auto WrapIndexAsPosition(
    mir::CompilationUnit& unit, mir::Builder block, const PositionMap& map,
    mir::ExprId index, std::int64_t shift) -> mir::ExprId {
  mir::ExprId position = BuildOrdinalPosition(unit, block, index);
  if (map.reversed) {
    position = Arithmetic(
        unit, block, mir::BinaryOp::kSub,
        BuildConstantPosition(unit, block, map.origin), position);
  } else if (map.origin != 0) {
    position = Arithmetic(
        unit, block, mir::BinaryOp::kSub, position,
        BuildConstantPosition(unit, block, map.origin));
  }
  if (map.step != 1) {
    position = Arithmetic(
        unit, block, mir::BinaryOp::kMul, position,
        BuildConstantPosition(unit, block, map.step));
  }
  if (shift != 0) {
    position = Arithmetic(
        unit, block, mir::BinaryOp::kAdd, position,
        BuildConstantPosition(unit, block, shift));
  }
  return position;
}

auto BuildSpanEnd(
    mir::CompilationUnit& unit, mir::Builder block, mir::ExprId start,
    mir::ExprId count, bool up) -> mir::ExprId {
  const mir::ExprId from = BuildOrdinalPosition(unit, block, start);
  const mir::ExprId lhs = BuildOrdinalPosition(unit, block, count);
  const mir::ExprId rhs = BuildConstantPosition(unit, block, 1);
  const mir::ExprId extent =
      Arithmetic(unit, block, mir::BinaryOp::kSub, lhs, rhs);
  return Arithmetic(
      unit, block, up ? mir::BinaryOp::kAdd : mir::BinaryOp::kSub, from,
      extent);
}

}  // namespace lyra::lowering::hir_to_mir
