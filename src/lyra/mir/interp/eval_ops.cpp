#include "lyra/mir/interp/eval_ops.hpp"

#include <algorithm>
#include <bit>
#include <cstdint>
#include <format>

#include "lyra/common/internal_error.hpp"
#include "lyra/mir/interp/runtime_integral_ops.hpp"
#include "lyra/mir/interp/runtime_real_ops.hpp"
#include "lyra/mir/interp/runtime_value.hpp"
#include "lyra/mir/operator.hpp"
#include "lyra/semantic/format.hpp"

namespace lyra::mir::interp {

namespace {

// Evaluate string comparison operations.
// Strings support ==, !=, <, <=, >, >= with lexicographic ordering.
// Returns 1-bit RuntimeIntegral (matches integral comparison result type).
auto EvalStringBinary(
    BinaryOp op, const RuntimeString& lhs, const RuntimeString& rhs)
    -> RuntimeValue {
  bool result = false;

  switch (op) {
    case BinaryOp::kEqual:
      result = (lhs.value == rhs.value);
      break;
    case BinaryOp::kNotEqual:
      result = (lhs.value != rhs.value);
      break;
    case BinaryOp::kLessThan:
      result = (lhs.value < rhs.value);  // Byte-wise lexicographic
      break;
    case BinaryOp::kLessThanEqual:
      result = (lhs.value <= rhs.value);
      break;
    case BinaryOp::kGreaterThan:
      result = (lhs.value > rhs.value);
      break;
    case BinaryOp::kGreaterThanEqual:
      result = (lhs.value >= rhs.value);
      break;
    default:
      throw common::InternalError(
          "EvalStringBinary",
          std::format("operator {} not supported for strings", ToString(op)));
  }

  return MakeIntegral(result ? 1 : 0, 1);
}

// Evaluate real binary operations.
// Arithmetic returns RuntimeReal; comparison/logical returns 1-bit integral.
auto EvalRealBinary(BinaryOp op, const RuntimeReal& lhs, const RuntimeReal& rhs)
    -> RuntimeValue {
  switch (op) {
    // Arithmetic
    case BinaryOp::kAdd:
      return MakeReal(RealAdd(lhs, rhs).value);
    case BinaryOp::kSubtract:
      return MakeReal(RealSub(lhs, rhs).value);
    case BinaryOp::kMultiply:
      return MakeReal(RealMul(lhs, rhs).value);
    case BinaryOp::kDivide:
    case BinaryOp::kDivideSigned:
      return MakeReal(RealDiv(lhs, rhs).value);

    // Comparisons
    case BinaryOp::kEqual:
    case BinaryOp::kCaseEqual:
      return RealEq(lhs, rhs);
    case BinaryOp::kNotEqual:
    case BinaryOp::kCaseNotEqual:
      return RealNe(lhs, rhs);
    case BinaryOp::kLessThan:
    case BinaryOp::kLessThanSigned:
      return RealLt(lhs, rhs);
    case BinaryOp::kLessThanEqual:
    case BinaryOp::kLessThanEqualSigned:
      return RealLe(lhs, rhs);
    case BinaryOp::kGreaterThan:
    case BinaryOp::kGreaterThanSigned:
      return RealGt(lhs, rhs);
    case BinaryOp::kGreaterThanEqual:
    case BinaryOp::kGreaterThanEqualSigned:
      return RealGe(lhs, rhs);

    // Logical
    case BinaryOp::kLogicalAnd:
      return RealLogicalAnd(lhs, rhs);
    case BinaryOp::kLogicalOr:
      return RealLogicalOr(lhs, rhs);

    // Power and math binary
    case BinaryOp::kPower:
      return MakeReal(RealPower(lhs, rhs).value);
    case BinaryOp::kAtan2:
      return MakeReal(RealAtan2(lhs, rhs).value);
    case BinaryOp::kHypot:
      return MakeReal(RealHypot(lhs, rhs).value);

    // Not supported for reals
    case BinaryOp::kMod:
    case BinaryOp::kModSigned:
    case BinaryOp::kBitwiseAnd:
    case BinaryOp::kBitwiseOr:
    case BinaryOp::kBitwiseXor:
    case BinaryOp::kBitwiseXnor:
    case BinaryOp::kLogicalImplication:
    case BinaryOp::kLogicalEquivalence:
    case BinaryOp::kWildcardEqual:
    case BinaryOp::kWildcardNotEqual:
    case BinaryOp::kCaseZMatch:
    case BinaryOp::kCaseXMatch:
    case BinaryOp::kLogicalShiftLeft:
    case BinaryOp::kLogicalShiftRight:
    case BinaryOp::kArithmeticShiftLeft:
    case BinaryOp::kArithmeticShiftRight:
      throw common::InternalError(
          "EvalRealBinary",
          std::format("operator {} not supported for real type", ToString(op)));
  }

  throw common::InternalError(
      "EvalRealBinary",
      std::format("unknown binary operation: {}", ToString(op)));
}

// Evaluate shortreal binary operations (32-bit float).
// Arithmetic returns RuntimeShortReal; comparison/logical returns 1-bit
// integral.
auto EvalShortRealBinary(
    BinaryOp op, const RuntimeShortReal& lhs, const RuntimeShortReal& rhs)
    -> RuntimeValue {
  switch (op) {
    // Arithmetic
    case BinaryOp::kAdd:
      return MakeShortReal(ShortRealAdd(lhs, rhs).value);
    case BinaryOp::kSubtract:
      return MakeShortReal(ShortRealSub(lhs, rhs).value);
    case BinaryOp::kMultiply:
      return MakeShortReal(ShortRealMul(lhs, rhs).value);
    case BinaryOp::kDivide:
    case BinaryOp::kDivideSigned:
      return MakeShortReal(ShortRealDiv(lhs, rhs).value);

    // Comparisons
    case BinaryOp::kEqual:
    case BinaryOp::kCaseEqual:
      return ShortRealEq(lhs, rhs);
    case BinaryOp::kNotEqual:
    case BinaryOp::kCaseNotEqual:
      return ShortRealNe(lhs, rhs);
    case BinaryOp::kLessThan:
    case BinaryOp::kLessThanSigned:
      return ShortRealLt(lhs, rhs);
    case BinaryOp::kLessThanEqual:
    case BinaryOp::kLessThanEqualSigned:
      return ShortRealLe(lhs, rhs);
    case BinaryOp::kGreaterThan:
    case BinaryOp::kGreaterThanSigned:
      return ShortRealGt(lhs, rhs);
    case BinaryOp::kGreaterThanEqual:
    case BinaryOp::kGreaterThanEqualSigned:
      return ShortRealGe(lhs, rhs);

    // Logical
    case BinaryOp::kLogicalAnd:
      return ShortRealLogicalAnd(lhs, rhs);
    case BinaryOp::kLogicalOr:
      return ShortRealLogicalOr(lhs, rhs);

    // Power
    case BinaryOp::kPower:
      return MakeShortReal(ShortRealPower(lhs, rhs).value);

    // Not supported for shortreals
    case BinaryOp::kMod:
    case BinaryOp::kModSigned:
    case BinaryOp::kBitwiseAnd:
    case BinaryOp::kBitwiseOr:
    case BinaryOp::kBitwiseXor:
    case BinaryOp::kBitwiseXnor:
    case BinaryOp::kLogicalImplication:
    case BinaryOp::kLogicalEquivalence:
    case BinaryOp::kWildcardEqual:
    case BinaryOp::kWildcardNotEqual:
    case BinaryOp::kCaseZMatch:
    case BinaryOp::kCaseXMatch:
    case BinaryOp::kLogicalShiftLeft:
    case BinaryOp::kLogicalShiftRight:
    case BinaryOp::kArithmeticShiftLeft:
    case BinaryOp::kArithmeticShiftRight:
    case BinaryOp::kAtan2:
    case BinaryOp::kHypot:
      throw common::InternalError(
          "EvalShortRealBinary",
          std::format(
              "operator {} not supported for shortreal type", ToString(op)));
  }

  throw common::InternalError(
      "EvalShortRealBinary",
      std::format("unknown binary operation: {}", ToString(op)));
}

}  // namespace

auto EvalBinary(BinaryOp op, const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue {
  // String comparison
  if (IsString(lhs) && IsString(rhs)) {
    return EvalStringBinary(op, AsString(lhs), AsString(rhs));
  }

  // Mixed string/non-string - should never happen (Slang type-checks)
  if (IsString(lhs) || IsString(rhs)) {
    throw common::InternalError(
        "EvalBinary", std::format(
                          "cannot mix string and non-string operands "
                          "(op={}, lhs={}, rhs={})",
                          ToString(op), lhs.index(), rhs.index()));
  }

  // Real operations
  if (IsReal(lhs) && IsReal(rhs)) {
    return EvalRealBinary(op, AsReal(lhs), AsReal(rhs));
  }

  // Shortreal operations
  if (IsShortReal(lhs) && IsShortReal(rhs)) {
    return EvalShortRealBinary(op, AsShortReal(lhs), AsShortReal(rhs));
  }

  // Mixed real/shortreal/non-float - should never happen (MIR has explicit
  // casts)
  if (IsReal(lhs) || IsReal(rhs) || IsShortReal(lhs) || IsShortReal(rhs)) {
    throw common::InternalError(
        "EvalBinary", std::format(
                          "cannot mix real/shortreal and other operands "
                          "(op={}, lhs={}, rhs={})",
                          ToString(op), lhs.index(), rhs.index()));
  }

  if (!IsIntegral(lhs) || !IsIntegral(rhs)) {
    throw common::InternalError(
        "EvalBinary", "binary operation requires integral operands");
  }

  const auto& lhs_int = AsIntegral(lhs);
  const auto& rhs_int = AsIntegral(rhs);

  // Arithmetic/bitwise ops use max width; shifts use lhs width (per SV rules)
  uint32_t arith_width = std::max(lhs_int.bit_width, rhs_int.bit_width);

  switch (op) {
    case BinaryOp::kAdd:
      return IntegralAdd(lhs_int, rhs_int, arith_width);

    case BinaryOp::kSubtract:
      return IntegralSub(lhs_int, rhs_int, arith_width);

    case BinaryOp::kMultiply:
      return IntegralMul(lhs_int, rhs_int, arith_width);

    case BinaryOp::kDivide:
      return IntegralDiv(lhs_int, rhs_int, arith_width, false);

    case BinaryOp::kMod:
      return IntegralMod(lhs_int, rhs_int, arith_width, false);

    case BinaryOp::kDivideSigned:
      return IntegralDiv(lhs_int, rhs_int, arith_width, true);

    case BinaryOp::kModSigned:
      return IntegralMod(lhs_int, rhs_int, arith_width, true);

    case BinaryOp::kPower:
    case BinaryOp::kAtan2:
    case BinaryOp::kHypot:
      throw common::InternalError(
          "EvalBinary",
          std::format(
              "operator {} not supported for integral type", ToString(op)));

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

    case BinaryOp::kCaseZMatch:
      return IntegralCaseZMatch(lhs_int, rhs_int);

    case BinaryOp::kCaseXMatch:
      return IntegralCaseXMatch(lhs_int, rhs_int);

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
      "EvalBinary", std::format("unknown binary operation: {}", ToString(op)));
}

auto EvalUnary(
    UnaryOp op, const RuntimeValue& operand, TypeId result_type,
    const TypeArena& types) -> RuntimeValue {
  // Real operands
  if (IsReal(operand)) {
    const auto& op_real = AsReal(operand);
    switch (op) {
      case UnaryOp::kPlus:
        return MakeReal(RealPlus(op_real).value);
      case UnaryOp::kMinus:
        return MakeReal(RealNeg(op_real).value);
      case UnaryOp::kLogicalNot:
        return RealLogicalNot(op_real);
      case UnaryOp::kLn:
        return MakeReal(RealLn(op_real).value);
      case UnaryOp::kLog10:
        return MakeReal(RealLog10(op_real).value);
      case UnaryOp::kExp:
        return MakeReal(RealExp(op_real).value);
      case UnaryOp::kSqrt:
        return MakeReal(RealSqrt(op_real).value);
      case UnaryOp::kFloor:
        return MakeReal(RealFloor(op_real).value);
      case UnaryOp::kCeil:
        return MakeReal(RealCeil(op_real).value);
      case UnaryOp::kSin:
        return MakeReal(RealSin(op_real).value);
      case UnaryOp::kCos:
        return MakeReal(RealCos(op_real).value);
      case UnaryOp::kTan:
        return MakeReal(RealTan(op_real).value);
      case UnaryOp::kAsin:
        return MakeReal(RealAsin(op_real).value);
      case UnaryOp::kAcos:
        return MakeReal(RealAcos(op_real).value);
      case UnaryOp::kAtan:
        return MakeReal(RealAtan(op_real).value);
      case UnaryOp::kSinh:
        return MakeReal(RealSinh(op_real).value);
      case UnaryOp::kCosh:
        return MakeReal(RealCosh(op_real).value);
      case UnaryOp::kTanh:
        return MakeReal(RealTanh(op_real).value);
      case UnaryOp::kAsinh:
        return MakeReal(RealAsinh(op_real).value);
      case UnaryOp::kAcosh:
        return MakeReal(RealAcosh(op_real).value);
      case UnaryOp::kAtanh:
        return MakeReal(RealAtanh(op_real).value);
      default:
        throw common::InternalError(
            "EvalUnary",
            std::format(
                "operator {} not supported for real type", ToString(op)));
    }
  }

  // Shortreal operands: only support +, -, and !
  if (IsShortReal(operand)) {
    const auto& op_shortreal = AsShortReal(operand);
    switch (op) {
      case UnaryOp::kPlus:
        return MakeShortReal(ShortRealPlus(op_shortreal).value);
      case UnaryOp::kMinus:
        return MakeShortReal(ShortRealNeg(op_shortreal).value);
      case UnaryOp::kLogicalNot:
        return ShortRealLogicalNot(op_shortreal);
      default:
        throw common::InternalError(
            "EvalUnary",
            std::format(
                "operator {} not supported for shortreal type", ToString(op)));
    }
  }

  if (!IsIntegral(operand)) {
    throw common::InternalError(
        "EvalUnary", "unary operation requires integral operand");
  }

  const auto& op_int = AsIntegral(operand);
  uint32_t width = op_int.bit_width;

  switch (op) {
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

    case UnaryOp::kIsKnown:
      return MakeIntegral(op_int.IsKnown() ? 1 : 0, 1);

    case UnaryOp::kClog2: {
      uint32_t result_width = PackedBitWidth(types[result_type], types);
      if (!op_int.IsKnown()) {
        return MakeUnknownIntegral(result_width);
      }
      if (op_int.IsZero()) {
        return MakeIntegral(0, result_width);
      }
      auto one = std::get<RuntimeIntegral>(MakeIntegral(1, op_int.bit_width));
      auto n_minus_1 = IntegralSub(op_int, one, op_int.bit_width);
      if (n_minus_1.IsZero()) {
        return MakeIntegral(0, result_width);
      }
      int highest_bit = -1;
      for (int i = static_cast<int>(n_minus_1.value.size()) - 1; i >= 0; --i) {
        if (n_minus_1.value[i] != 0) {
          highest_bit = i * 64 + (63 - std::countl_zero(n_minus_1.value[i]));
          break;
        }
      }
      return MakeIntegral(highest_bit + 1, result_width);
    }

    case UnaryOp::kLn:
    case UnaryOp::kLog10:
    case UnaryOp::kExp:
    case UnaryOp::kSqrt:
    case UnaryOp::kFloor:
    case UnaryOp::kCeil:
    case UnaryOp::kSin:
    case UnaryOp::kCos:
    case UnaryOp::kTan:
    case UnaryOp::kAsin:
    case UnaryOp::kAcos:
    case UnaryOp::kAtan:
    case UnaryOp::kSinh:
    case UnaryOp::kCosh:
    case UnaryOp::kTanh:
    case UnaryOp::kAsinh:
    case UnaryOp::kAcosh:
    case UnaryOp::kAtanh:
      throw common::InternalError(
          "EvalUnary",
          std::format(
              "operator {} requires real operand, got integral", ToString(op)));
  }

  throw common::InternalError(
      "EvalUnary", std::format("unknown unary operation: {}", ToString(op)));
}

auto EvalCast(
    const RuntimeValue& operand, const Type& source_type,
    const Type& target_type, const TypeArena& arena) -> RuntimeValue {
  bool src_is_real = source_type.Kind() == TypeKind::kReal;
  bool src_is_shortreal = source_type.Kind() == TypeKind::kShortReal;
  bool tgt_is_real = target_type.Kind() == TypeKind::kReal;
  bool tgt_is_shortreal = target_type.Kind() == TypeKind::kShortReal;
  bool src_is_packed = IsPacked(source_type);
  bool tgt_is_packed = IsPacked(target_type);

  // Real -> Packed conversion
  if (src_is_real && tgt_is_packed) {
    if (!IsReal(operand)) {
      throw common::InternalError(
          "EvalCast", "real source type but operand is not real");
    }
    uint32_t target_width = PackedBitWidth(target_type, arena);
    bool target_signed = IsPackedSigned(target_type, arena);
    return RealToIntegral(AsReal(operand), target_width, target_signed);
  }

  // Packed -> Real conversion
  if (src_is_packed && tgt_is_real) {
    if (!IsIntegral(operand)) {
      throw common::InternalError(
          "EvalCast", "packed source type but operand is not integral");
    }
    bool src_is_signed = IsPackedSigned(source_type, arena);
    return MakeReal(IntegralToReal(AsIntegral(operand), src_is_signed).value);
  }

  // Real -> Real (identity, but validates operand type)
  if (src_is_real && tgt_is_real) {
    if (!IsReal(operand)) {
      throw common::InternalError(
          "EvalCast", "real source type but operand is not real");
    }
    return Clone(operand);
  }

  // Shortreal -> Packed conversion
  if (src_is_shortreal && tgt_is_packed) {
    if (!IsShortReal(operand)) {
      throw common::InternalError(
          "EvalCast", "shortreal source type but operand is not shortreal");
    }
    uint32_t target_width = PackedBitWidth(target_type, arena);
    bool target_signed = IsPackedSigned(target_type, arena);
    return ShortRealToIntegral(
        AsShortReal(operand), target_width, target_signed);
  }

  // Packed -> Shortreal conversion
  if (src_is_packed && tgt_is_shortreal) {
    if (!IsIntegral(operand)) {
      throw common::InternalError(
          "EvalCast", "packed source type but operand is not integral");
    }
    bool src_is_signed = IsPackedSigned(source_type, arena);
    return MakeShortReal(
        IntegralToShortReal(AsIntegral(operand), src_is_signed).value);
  }

  // Shortreal -> Shortreal (identity)
  if (src_is_shortreal && tgt_is_shortreal) {
    if (!IsShortReal(operand)) {
      throw common::InternalError(
          "EvalCast", "shortreal source type but operand is not shortreal");
    }
    return Clone(operand);
  }

  // Shortreal -> Real conversion
  if (src_is_shortreal && tgt_is_real) {
    if (!IsShortReal(operand)) {
      throw common::InternalError(
          "EvalCast", "shortreal source type but operand is not shortreal");
    }
    return MakeReal(ShortRealToReal(AsShortReal(operand)).value);
  }

  // Real -> Shortreal conversion
  if (src_is_real && tgt_is_shortreal) {
    if (!IsReal(operand)) {
      throw common::InternalError(
          "EvalCast", "real source type but operand is not real");
    }
    return MakeShortReal(RealToShortReal(AsReal(operand)).value);
  }

  // Packed -> String conversion (byte extraction)
  bool tgt_is_string = target_type.Kind() == TypeKind::kString;
  if (src_is_packed && tgt_is_string) {
    if (!IsIntegral(operand)) {
      throw common::InternalError(
          "EvalCast", "packed source type but operand is not integral");
    }
    return MakeString(semantic::PackedToStringBytes(AsIntegral(operand)));
  }

  // String -> Packed conversion (byte packing)
  bool src_is_string = source_type.Kind() == TypeKind::kString;
  if (src_is_string && tgt_is_packed) {
    if (!IsString(operand)) {
      throw common::InternalError(
          "EvalCast", "string source type but operand is not string");
    }
    uint32_t target_width = PackedBitWidth(target_type, arena);
    return StringBytesToIntegral(AsString(operand).value, target_width);
  }

  // Packed -> Packed conversion (existing logic)
  if (!IsIntegral(operand)) {
    throw common::InternalError(
        "EvalCast", "cast operation requires integral operand");
  }
  if (!src_is_packed) {
    throw common::InternalError("EvalCast", "source type must be packed");
  }
  if (!tgt_is_packed) {
    throw common::InternalError("EvalCast", "target type must be packed");
  }

  const auto& op_int = AsIntegral(operand);
  bool src_is_signed = IsPackedSigned(source_type, arena);
  uint32_t target_width = PackedBitWidth(target_type, arena);

  // For 4-state -> 2-state conversion, X/Z bits become 0.
  // We create a clean 2-state value by masking out X/Z from the value bits.
  RuntimeIntegral clean_src = op_int;
  if (IsPackedFourState(source_type, arena)) {
    // X/Z bits become 0: clear value bits where X or Z is set
    for (size_t i = 0; i < clean_src.value.size(); ++i) {
      uint64_t xz_mask = 0;
      if (i < clean_src.x_mask.size()) {
        xz_mask |= clean_src.x_mask[i];
      }
      if (i < clean_src.z_mask.size()) {
        xz_mask |= clean_src.z_mask[i];
      }
      clean_src.value[i] &= ~xz_mask;
    }
    // Clear X/Z masks (result is 2-state)
    std::ranges::fill(clean_src.x_mask, 0);
    std::ranges::fill(clean_src.z_mask, 0);
  }
  // Note: 2-state -> 4-state is lossless (no X/Z bits introduced)

  // Cast = resize bits using source signedness and target width.
  // Target signedness does not affect the bit pattern; it only affects how
  // downstream operations interpret the result.
  return IntegralResize2State(clean_src, src_is_signed, target_width);
}

namespace {

// Check if type is a packed integral (kIntegral or kPackedArray, not
// kPackedStruct)
auto IsPackedIntegral(const Type& type) -> bool {
  return type.Kind() == TypeKind::kIntegral ||
         type.Kind() == TypeKind::kPackedArray;
}

}  // namespace

auto EvalBitCast(
    const RuntimeValue& operand, TypeId source_type, TypeId target_type,
    const TypeArena& arena) -> RuntimeValue {
  const Type& src = arena[source_type];
  const Type& tgt = arena[target_type];

  // Case 1: real -> packed integral ($realtobits)
  if (src.Kind() == TypeKind::kReal && IsPackedIntegral(tgt)) {
    if (!IsReal(operand)) {
      throw common::InternalError(
          "EvalBitCast", "real source type but operand is not real");
    }
    double val = AsReal(operand).value;
    auto bits = std::bit_cast<uint64_t>(val);
    return std::get<RuntimeIntegral>(MakeIntegral(bits, 64));
  }

  // Case 2: packed integral -> real ($bitstoreal)
  if (IsPackedIntegral(src) && tgt.Kind() == TypeKind::kReal) {
    if (!IsIntegral(operand)) {
      throw common::InternalError(
          "EvalBitCast", "integral source type but operand is not integral");
    }
    const auto& integral = AsIntegral(operand);
    auto bits = integral.value.empty() ? 0ULL : integral.value[0];
    auto val = std::bit_cast<double>(bits);
    return MakeReal(val);
  }

  // Case 3: shortreal -> packed integral ($shortrealtobits)
  if (src.Kind() == TypeKind::kShortReal && IsPackedIntegral(tgt)) {
    if (!IsShortReal(operand)) {
      throw common::InternalError(
          "EvalBitCast", "shortreal source type but operand is not shortreal");
    }
    float val = AsShortReal(operand).value;
    auto bits = std::bit_cast<uint32_t>(val);
    return std::get<RuntimeIntegral>(MakeIntegral(bits, 32));
  }

  // Case 4: packed integral -> shortreal ($bitstoshortreal)
  if (IsPackedIntegral(src) && tgt.Kind() == TypeKind::kShortReal) {
    if (!IsIntegral(operand)) {
      throw common::InternalError(
          "EvalBitCast", "integral source type but operand is not integral");
    }
    const auto& integral = AsIntegral(operand);
    auto bits =
        static_cast<uint32_t>(integral.value.empty() ? 0 : integral.value[0]);
    auto val = std::bit_cast<float>(bits);
    return MakeShortReal(val);
  }

  // HIR guarantees validity - unreachable
  throw common::InternalError(
      "EvalBitCast",
      std::format("invalid bitcast: {} -> {}", ToString(src), ToString(tgt)));
}

}  // namespace lyra::mir::interp
