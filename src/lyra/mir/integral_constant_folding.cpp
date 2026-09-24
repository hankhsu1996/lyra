#include "lyra/mir/integral_constant_folding.hpp"

#include <cstdint>
#include <optional>
#include <variant>
#include <vector>

#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/mir/unary_op.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::mir {

namespace {

using value::PackedArray;

auto ShapeOf(const CompilationUnit& unit, TypeId type)
    -> const PackedArrayType* {
  const Type& ty = unit.types.Get(type);
  return ty.IsIntegralPacked() ? &ty.PackedShape() : nullptr;
}

auto IsFourState(const PackedArrayType& shape) -> bool {
  return shape.state_kind == IntegralStateKind::kFourState;
}

auto IsSigned(const PackedArrayType& shape) -> bool {
  return shape.signedness == Signedness::kSigned;
}

// The value a constant operand holds, or nothing where the operand is not a
// constant of the unit.
auto ConstantOperand(const CompilationUnit& unit, const Block& block, ExprId id)
    -> std::optional<PackedArray> {
  const Expr& operand = block.exprs.Get(id);
  const auto* reference = std::get_if<ReferenceExpr>(&operand.data);
  if (reference == nullptr) {
    return std::nullopt;
  }
  const auto* constant = std::get_if<IntegralConstantRef>(&reference->target);
  const PackedArrayType* shape = ShapeOf(unit, operand.type);
  if (constant == nullptr || shape == nullptr) {
    return std::nullopt;
  }
  const IntegralConstantDecl& decl =
      unit.integral_constants.Get(constant->constant);
  return PackedArray::FromWords(
      decl.value.value_words, decl.value.state_words, shape->BitWidth(),
      IsSigned(*shape), IsFourState(*shape));
}

// What an evaluated value is as an entry of the pool, where it is the value of
// the node's own type; nothing where the library answered at another one.
auto AsConstant(
    const CompilationUnit& unit, TypeId result,
    const std::optional<PackedArray>& evaluated)
    -> std::optional<IntegralConstant> {
  const PackedArrayType* shape = ShapeOf(unit, result);
  if (!evaluated || shape == nullptr ||
      evaluated->BitWidth() != shape->BitWidth() ||
      evaluated->IsSigned() != IsSigned(*shape) ||
      evaluated->IsFourState() != IsFourState(*shape)) {
    return std::nullopt;
  }
  const auto value_words = evaluated->ValueWords();
  const auto unknown_words = evaluated->UnknownWords();
  return IntegralConstant{
      .value_words = {value_words.begin(), value_words.end()},
      .state_words = IsFourState(*shape)
                         ? std::vector<std::uint64_t>(
                               unknown_words.begin(), unknown_words.end())
                         : std::vector<std::uint64_t>{}};
}

auto EvaluateBinary(BinaryOp op, const PackedArray& lhs, const PackedArray& rhs)
    -> PackedArray {
  switch (op) {
    case BinaryOp::kAdd:
      return lhs + rhs;
    case BinaryOp::kSub:
      return lhs - rhs;
    case BinaryOp::kMul:
      return lhs * rhs;
    case BinaryOp::kDiv:
      return lhs / rhs;
    case BinaryOp::kMod:
      return lhs % rhs;
    case BinaryOp::kBitwiseAnd:
      return lhs & rhs;
    case BinaryOp::kBitwiseOr:
      return lhs | rhs;
    case BinaryOp::kBitwiseXor:
      return lhs ^ rhs;
    case BinaryOp::kEquality:
      return lhs == rhs;
    case BinaryOp::kInequality:
      return lhs != rhs;
    case BinaryOp::kGreaterEqual:
      return lhs >= rhs;
    case BinaryOp::kGreaterThan:
      return lhs > rhs;
    case BinaryOp::kLessEqual:
      return lhs <= rhs;
    case BinaryOp::kLessThan:
      return lhs < rhs;
    case BinaryOp::kLogicalAnd:
      return lhs && rhs;
    case BinaryOp::kLogicalOr:
      return lhs || rhs;
  }
  return lhs;
}

auto EvaluateUnary(UnaryOp op, const PackedArray& operand) -> PackedArray {
  switch (op) {
    case UnaryOp::kMinus:
      return -operand;
    case UnaryOp::kBitwiseNot:
      return ~operand;
    case UnaryOp::kLogicalNot:
      return !operand;
  }
  return operand;
}

}  // namespace

auto FoldBinary(
    const CompilationUnit& unit, const Block& block, BinaryOp op, ExprId lhs,
    ExprId rhs, TypeId result) -> std::optional<IntegralConstant> {
  const std::optional<PackedArray> left = ConstantOperand(unit, block, lhs);
  const std::optional<PackedArray> right = ConstantOperand(unit, block, rhs);
  // The library holds the two sides of an operator to one width and one state
  // domain; a pair that differs is a conversion the lowering owed.
  if (!left || !right || left->BitWidth() != right->BitWidth() ||
      left->IsFourState() != right->IsFourState()) {
    return std::nullopt;
  }
  return AsConstant(unit, result, EvaluateBinary(op, *left, *right));
}

auto FoldUnary(
    const CompilationUnit& unit, const Block& block, UnaryOp op, ExprId operand,
    TypeId result) -> std::optional<IntegralConstant> {
  const std::optional<PackedArray> value =
      ConstantOperand(unit, block, operand);
  if (!value) {
    return std::nullopt;
  }
  return AsConstant(unit, result, EvaluateUnary(op, *value));
}

auto FoldConversion(
    const CompilationUnit& unit, const Block& block, ExprId operand,
    TypeId result) -> std::optional<IntegralConstant> {
  const std::optional<PackedArray> value =
      ConstantOperand(unit, block, operand);
  const PackedArrayType* shape = ShapeOf(unit, result);
  if (!value || shape == nullptr) {
    return std::nullopt;
  }
  return AsConstant(
      unit, result,
      PackedArray::ConvertFrom(
          *value, shape->BitWidth(), IsSigned(*shape), IsFourState(*shape)));
}

auto FoldPosition(
    const CompilationUnit& unit, const Block& block, ExprId index,
    TypeId result) -> std::optional<IntegralConstant> {
  const std::optional<PackedArray> value = ConstantOperand(unit, block, index);
  if (!value) {
    return std::nullopt;
  }
  return AsConstant(unit, result, PackedArray::ToPosition(*value));
}

}  // namespace lyra::mir
