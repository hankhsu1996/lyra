#pragma once

namespace lyra::mir {

enum class BinaryOp {
  kAdd,
  kSub,
  kMul,
  kDiv,
  kMod,
  kPower,
  kBitwiseAnd,
  kBitwiseOr,
  kBitwiseXor,
  kBitwiseXnor,
  kEquality,
  kInequality,
  kCaseEquality,
  kCaseInequality,
  kWildcardEquality,
  kWildcardInequality,
  kGreaterEqual,
  kGreaterThan,
  kLessEqual,
  kLessThan,
  kLogicalAnd,
  kLogicalOr,
  kLogicalImplication,
  kLogicalEquivalence,
  kShiftLeft,
  kLogicalShiftRight,
  kArithmeticShiftRight,
};

}  // namespace lyra::mir
