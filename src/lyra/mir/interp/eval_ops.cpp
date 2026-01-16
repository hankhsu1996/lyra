#include "lyra/mir/interp/eval_ops.hpp"

#include <algorithm>
#include <bit>
#include <cstdint>
#include <format>

#include "lyra/common/internal_error.hpp"
#include "lyra/mir/interp/runtime_integral_ops.hpp"
#include "lyra/mir/interp/runtime_value.hpp"
#include "lyra/mir/operator.hpp"

namespace lyra::mir::interp {

auto EvalBinary(int op, const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue {
  if (!IsIntegral(lhs) || !IsIntegral(rhs)) {
    throw common::InternalError(
        "EvalBinary", "binary operation requires integral operands");
  }

  const auto& lhs_int = AsIntegral(lhs);
  const auto& rhs_int = AsIntegral(rhs);

  // Arithmetic/bitwise ops use max width; shifts use lhs width (per SV rules)
  uint32_t arith_width = std::max(lhs_int.bit_width, rhs_int.bit_width);

  auto bin_op = static_cast<BinaryOp>(op);
  switch (bin_op) {
    case BinaryOp::kAdd:
      return IntegralAdd(lhs_int, rhs_int, arith_width);

    case BinaryOp::kSubtract:
      return IntegralSub(lhs_int, rhs_int, arith_width);

    case BinaryOp::kMultiply:
      return IntegralMul(lhs_int, rhs_int, arith_width);

    case BinaryOp::kDivide:
      return IntegralDiv(lhs_int, rhs_int, arith_width);

    case BinaryOp::kMod:
      return IntegralMod(lhs_int, rhs_int, arith_width);

    case BinaryOp::kPower:
      throw common::InternalError(
          "EvalBinary", "power operation not supported");

    case BinaryOp::kBitwiseAnd:
      return IntegralAnd(lhs_int, rhs_int, arith_width);

    case BinaryOp::kBitwiseOr:
      return IntegralOr(lhs_int, rhs_int, arith_width);

    case BinaryOp::kBitwiseXor:
      return IntegralXor(lhs_int, rhs_int, arith_width);

    case BinaryOp::kBitwiseXnor: {
      auto xor_result = IntegralXor(lhs_int, rhs_int, arith_width);
      return IntegralNot(xor_result, arith_width);
    }

    case BinaryOp::kLogicalAnd:
      return IntegralLogicalAnd(lhs_int, rhs_int);

    case BinaryOp::kLogicalOr:
      return IntegralLogicalOr(lhs_int, rhs_int);

    case BinaryOp::kLogicalImplication:
      // a -> b = !a || b
      return IntegralLogicalOr(IntegralLogicalNot(lhs_int), rhs_int);

    case BinaryOp::kLogicalEquivalence: {
      // a <-> b = (a -> b) && (b -> a)
      auto a_implies_b =
          IntegralLogicalOr(IntegralLogicalNot(lhs_int), rhs_int);
      auto b_implies_a =
          IntegralLogicalOr(IntegralLogicalNot(rhs_int), lhs_int);
      return IntegralLogicalAnd(a_implies_b, b_implies_a);
    }

    case BinaryOp::kEqual:
      return IntegralEq(lhs_int, rhs_int);

    case BinaryOp::kNotEqual:
      return IntegralNe(lhs_int, rhs_int);

    case BinaryOp::kCaseEqual:
      // === is 4-state aware equality (X matches X, Z matches Z)
      // Simplified: treat as regular equality for now
      return IntegralEq(lhs_int, rhs_int);

    case BinaryOp::kCaseNotEqual:
      return IntegralNe(lhs_int, rhs_int);

    case BinaryOp::kWildcardEqual:
      throw common::InternalError(
          "EvalBinary", "wildcard equality not supported");

    case BinaryOp::kWildcardNotEqual:
      throw common::InternalError(
          "EvalBinary", "wildcard inequality not supported");

    case BinaryOp::kLessThan:
      return IntegralLt(lhs_int, rhs_int, false);

    case BinaryOp::kLessThanEqual:
      return IntegralLe(lhs_int, rhs_int, false);

    case BinaryOp::kGreaterThan:
      return IntegralGt(lhs_int, rhs_int, false);

    case BinaryOp::kGreaterThanEqual:
      return IntegralGe(lhs_int, rhs_int, false);

    case BinaryOp::kLessThanSigned:
      return IntegralLt(lhs_int, rhs_int, true);

    case BinaryOp::kLessThanEqualSigned:
      return IntegralLe(lhs_int, rhs_int, true);

    case BinaryOp::kGreaterThanSigned:
      return IntegralGt(lhs_int, rhs_int, true);

    case BinaryOp::kGreaterThanEqualSigned:
      return IntegralGe(lhs_int, rhs_int, true);

    case BinaryOp::kLogicalShiftLeft:
      return IntegralShl(lhs_int, rhs_int, lhs_int.bit_width);

    case BinaryOp::kLogicalShiftRight:
      return IntegralShr(lhs_int, rhs_int, lhs_int.bit_width, false);

    case BinaryOp::kArithmeticShiftLeft:
      return IntegralShl(lhs_int, rhs_int, lhs_int.bit_width);

    case BinaryOp::kArithmeticShiftRight:
      return IntegralShr(lhs_int, rhs_int, lhs_int.bit_width, true);
  }

  throw common::InternalError(
      "EvalBinary", std::format("unknown binary operation: {}", op));
}

auto EvalUnary(int op, const RuntimeValue& operand) -> RuntimeValue {
  if (!IsIntegral(operand)) {
    throw common::InternalError(
        "EvalUnary", "unary operation requires integral operand");
  }

  const auto& op_int = AsIntegral(operand);
  uint32_t width = op_int.bit_width;

  auto unary_op = static_cast<UnaryOp>(op);
  switch (unary_op) {
    case UnaryOp::kPlus:
      return Clone(operand);

    case UnaryOp::kMinus:
      return IntegralNeg(op_int, width);

    case UnaryOp::kPreincrement:
    case UnaryOp::kPostincrement:
    case UnaryOp::kPredecrement:
    case UnaryOp::kPostdecrement:
      throw common::InternalError(
          "EvalUnary", "increment/decrement operators not supported");

    case UnaryOp::kLogicalNot:
      return IntegralLogicalNot(op_int);

    case UnaryOp::kBitwiseNot:
      return IntegralNot(op_int, width);

    case UnaryOp::kReductionAnd: {
      // All bits must be 1
      if (!op_int.IsKnown()) {
        return MakeUnknownIntegral(1);
      }
      return std::get<RuntimeIntegral>(
          MakeIntegral(op_int.IsAllOnes() ? 1 : 0, 1));
    }

    case UnaryOp::kReductionNand: {
      if (!op_int.IsKnown()) {
        return MakeUnknownIntegral(1);
      }
      return std::get<RuntimeIntegral>(
          MakeIntegral(op_int.IsAllOnes() ? 0 : 1, 1));
    }

    case UnaryOp::kReductionOr: {
      if (!op_int.IsKnown()) {
        return MakeUnknownIntegral(1);
      }
      return std::get<RuntimeIntegral>(
          MakeIntegral(op_int.IsZero() ? 0 : 1, 1));
    }

    case UnaryOp::kReductionNor: {
      if (!op_int.IsKnown()) {
        return MakeUnknownIntegral(1);
      }
      return std::get<RuntimeIntegral>(
          MakeIntegral(op_int.IsZero() ? 1 : 0, 1));
    }

    case UnaryOp::kReductionXor: {
      if (!op_int.IsKnown()) {
        return MakeUnknownIntegral(1);
      }
      // Count bits set
      int count = 0;
      for (uint64_t w : op_int.value) {
        count += std::popcount(w);
      }
      return std::get<RuntimeIntegral>(
          MakeIntegral((count % 2) != 0 ? 1 : 0, 1));
    }

    case UnaryOp::kReductionXnor: {
      if (!op_int.IsKnown()) {
        return MakeUnknownIntegral(1);
      }
      int count = 0;
      for (uint64_t w : op_int.value) {
        count += std::popcount(w);
      }
      return std::get<RuntimeIntegral>(
          MakeIntegral((count % 2) == 0 ? 1 : 0, 1));
    }
  }

  throw common::InternalError(
      "EvalUnary", std::format("unknown unary operation: {}", op));
}

}  // namespace lyra::mir::interp
