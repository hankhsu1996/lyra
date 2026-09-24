#include "lyra/lowering/hir_to_mir/select_position.hpp"

#include <cstdint>

#include "lyra/base/internal_error.hpp"
#include "lyra/lowering/hir_to_mir/integral_literal.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/integral_constant_folding.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

constexpr PositionMap kIdentity{
    .origin = 0, .reversed = false, .step = 1, .from_right = false};

// The type position arithmetic is done in: 64-bit signed four-state, so an
// index of any width shifts by a declared range without wrapping, and an index
// holding x or z stays unknown through every step.
auto PositionType(mir::CompilationUnit& unit) -> mir::TypeId {
  return unit.types.Intern(
      mir::Type{mir::PackedArrayType{
          .state_kind = mir::IntegralStateKind::kFourState,
          .signedness = mir::Signedness::kSigned,
          .dims = {mir::PackedRange{.left = 63, .right = 0}}}});
}

auto Arithmetic(
    mir::CompilationUnit& unit, mir::Block& block, mir::BinaryOp op,
    mir::ExprId lhs, mir::ExprId rhs) -> mir::ExprId {
  const mir::TypeId type = PositionType(unit);
  return block.exprs.Add(FoldedOr(
      unit, mir::FoldBinary(unit, block, op, lhs, rhs, type),
      mir::Expr{
          .data = mir::BinaryExpr{.op = op, .lhs = lhs, .rhs = rhs},
          .type = type}));
}

auto ToPosition(
    mir::CompilationUnit& unit, mir::Block& block, mir::ExprId index)
    -> mir::ExprId {
  const mir::TypeId type = PositionType(unit);
  return block.exprs.Add(FoldedOr(
      unit, mir::FoldPosition(unit, block, index, type),
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{.target = support::BuiltinFn::kToPosition},
                  .arguments = {index}},
          .type = type}));
}

}  // namespace

auto PositionMapOf(const mir::CompilationUnit& unit, mir::TypeId receiver)
    -> PositionMap {
  const mir::Type& type = unit.types.Get(receiver);
  if (type.IsIntegralPacked()) {
    const mir::PackedArrayType& shape = type.PackedShape();
    const mir::PackedRange& outer = shape.dims.front();
    return PositionMap{
        .origin = outer.right,
        .reversed = outer.IsAscending(),
        .step =
            static_cast<std::int64_t>(shape.BitWidth() / outer.ElementCount()),
        .from_right = true};
  }
  if (const auto* array = type.As<mir::UnpackedArrayType>()) {
    return PositionMap{
        .origin = array->dim.left,
        .reversed = !array->dim.IsAscending(),
        .step = 1,
        .from_right = false};
  }
  if (type.Is<mir::DynamicArrayType>() || type.Is<mir::QueueType>() ||
      type.Is<mir::StringType>()) {
    return kIdentity;
  }
  throw InternalError(
      "PositionMapOf: a select reaching by position has a receiver that "
      "numbers its parts, and this one does not");
}

auto BuildConstantPosition(
    mir::CompilationUnit& unit, mir::Block& block, std::int64_t position)
    -> mir::ExprId {
  return BuildIntegralLiteral(
      unit, block, PositionType(unit),
      mir::IntegralConstant{
          .value_words = {static_cast<std::uint64_t>(position)},
          .state_words = {0}});
}

auto WrapIndexAsPosition(
    mir::CompilationUnit& unit, mir::Block& block, const PositionMap& map,
    mir::ExprId index, std::int64_t shift) -> mir::ExprId {
  if (map.origin == 0 && !map.reversed && map.step == 1 && shift == 0) {
    return index;
  }
  mir::ExprId position = ToPosition(unit, block, index);
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
    mir::CompilationUnit& unit, mir::Block& block, mir::ExprId start,
    mir::ExprId count, bool up) -> mir::ExprId {
  const mir::ExprId from = ToPosition(unit, block, start);
  const mir::ExprId extent = Arithmetic(
      unit, block, mir::BinaryOp::kSub, ToPosition(unit, block, count),
      BuildConstantPosition(unit, block, 1));
  return Arithmetic(
      unit, block, up ? mir::BinaryOp::kAdd : mir::BinaryOp::kSub, from,
      extent);
}

}  // namespace lyra::lowering::hir_to_mir
