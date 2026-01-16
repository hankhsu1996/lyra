#include "lyra/mir/interp/eval_ops.hpp"

#include <algorithm>
#include <bit>
#include <cstdint>
#include <format>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/operator.hpp"
#include "lyra/mir/interp/runtime_integral_ops.hpp"
#include "lyra/mir/interp/runtime_value.hpp"

namespace lyra::mir::interp {

auto EvalBinary(int op, const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue {
  if (!IsIntegral(lhs) || !IsIntegral(rhs)) {
    throw common::InternalError(
        "EvalBinary", "binary operation requires integral operands");
  }

  const auto& lhs_int = AsIntegral(lhs);
  const auto& rhs_int = AsIntegral(rhs);

  // Result width is max of operand widths (simplified)
  uint32_t width = std::max(lhs_int.bit_width, rhs_int.bit_width);

  auto bin_op = static_cast<common::BinaryOp>(op);
  switch (bin_op) {
    case common::BinaryOp::kAdd:
      return IntegralAdd(lhs_int, rhs_int, width);

    case common::BinaryOp::kSubtract:
      return IntegralSub(lhs_int, rhs_int, width);

    case common::BinaryOp::kMultiply:
      return IntegralMul(lhs_int, rhs_int, width);

    case common::BinaryOp::kDivide:
      return IntegralDiv(lhs_int, rhs_int, width);

    case common::BinaryOp::kMod:
      return IntegralMod(lhs_int, rhs_int, width);

    case common::BinaryOp::kPower:
      throw common::InternalError(
          "EvalBinary", "power operation not supported");

    case common::BinaryOp::kBitwiseAnd:
      return IntegralAnd(lhs_int, rhs_int, width);

    case common::BinaryOp::kBitwiseOr:
      return IntegralOr(lhs_int, rhs_int, width);

    case common::BinaryOp::kBitwiseXor:
      return IntegralXor(lhs_int, rhs_int, width);

    case common::BinaryOp::kBitwiseXnor: {
      auto xor_result = IntegralXor(lhs_int, rhs_int, width);
      return IntegralNot(xor_result, width);
    }

    case common::BinaryOp::kLogicalAnd:
      return IntegralLogicalAnd(lhs_int, rhs_int);

    case common::BinaryOp::kLogicalOr:
      return IntegralLogicalOr(lhs_int, rhs_int);

    case common::BinaryOp::kLogicalImplication:
      // a -> b = !a || b
      return IntegralLogicalOr(IntegralLogicalNot(lhs_int), rhs_int);

    case common::BinaryOp::kLogicalEquivalence: {
      // a <-> b = (a -> b) && (b -> a)
      auto a_implies_b =
          IntegralLogicalOr(IntegralLogicalNot(lhs_int), rhs_int);
      auto b_implies_a =
          IntegralLogicalOr(IntegralLogicalNot(rhs_int), lhs_int);
      return IntegralLogicalAnd(a_implies_b, b_implies_a);
    }

    case common::BinaryOp::kEqual:
      return IntegralEq(lhs_int, rhs_int);

    case common::BinaryOp::kNotEqual:
      return IntegralNe(lhs_int, rhs_int);

    case common::BinaryOp::kCaseEqual:
      // === is 4-state aware equality (X matches X, Z matches Z)
      // Simplified: treat as regular equality for now
      return IntegralEq(lhs_int, rhs_int);

    case common::BinaryOp::kCaseNotEqual:
      return IntegralNe(lhs_int, rhs_int);

    case common::BinaryOp::kWildcardEqual:
      throw common::InternalError(
          "EvalBinary", "wildcard equality not supported");

    case common::BinaryOp::kWildcardNotEqual:
      throw common::InternalError(
          "EvalBinary", "wildcard inequality not supported");

    case common::BinaryOp::kLessThan:
      return IntegralLt(lhs_int, rhs_int, false);

    case common::BinaryOp::kLessThanEqual:
      return IntegralLe(lhs_int, rhs_int, false);

    case common::BinaryOp::kGreaterThan:
      return IntegralGt(lhs_int, rhs_int, false);

    case common::BinaryOp::kGreaterThanEqual:
      return IntegralGe(lhs_int, rhs_int, false);

    case common::BinaryOp::kLogicalShiftLeft:
      return IntegralShl(lhs_int, rhs_int, lhs_int.bit_width);

    case common::BinaryOp::kLogicalShiftRight:
      return IntegralShr(lhs_int, rhs_int, lhs_int.bit_width, false);

    case common::BinaryOp::kArithmeticShiftLeft:
      return IntegralShl(lhs_int, rhs_int, lhs_int.bit_width);

    case common::BinaryOp::kArithmeticShiftRight:
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

  auto unary_op = static_cast<common::UnaryOp>(op);
  switch (unary_op) {
    case common::UnaryOp::kPlus:
      return Clone(operand);

    case common::UnaryOp::kMinus:
      return IntegralNeg(op_int, width);

    case common::UnaryOp::kPreincrement:
    case common::UnaryOp::kPostincrement:
    case common::UnaryOp::kPredecrement:
    case common::UnaryOp::kPostdecrement:
      throw common::InternalError(
          "EvalUnary", "increment/decrement operators not supported");

    case common::UnaryOp::kLogicalNot:
      return IntegralLogicalNot(op_int);

    case common::UnaryOp::kBitwiseNot:
      return IntegralNot(op_int, width);

    case common::UnaryOp::kReductionAnd: {
      // All bits must be 1
      if (!op_int.IsKnown()) {
        return MakeUnknownIntegral(1);
      }
      return std::get<RuntimeIntegral>(
          MakeIntegral(op_int.IsAllOnes() ? 1 : 0, 1));
    }

    case common::UnaryOp::kReductionNand: {
      if (!op_int.IsKnown()) {
        return MakeUnknownIntegral(1);
      }
      return std::get<RuntimeIntegral>(
          MakeIntegral(op_int.IsAllOnes() ? 0 : 1, 1));
    }

    case common::UnaryOp::kReductionOr: {
      if (!op_int.IsKnown()) {
        return MakeUnknownIntegral(1);
      }
      return std::get<RuntimeIntegral>(
          MakeIntegral(op_int.IsZero() ? 0 : 1, 1));
    }

    case common::UnaryOp::kReductionNor: {
      if (!op_int.IsKnown()) {
        return MakeUnknownIntegral(1);
      }
      return std::get<RuntimeIntegral>(
          MakeIntegral(op_int.IsZero() ? 1 : 0, 1));
    }

    case common::UnaryOp::kReductionXor: {
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

    case common::UnaryOp::kReductionXnor: {
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
