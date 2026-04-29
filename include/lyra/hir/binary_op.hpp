#pragma once

namespace lyra::hir {

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
  kLogicalShiftLeft,
  kLogicalShiftRight,
  kArithmeticShiftLeft,
  kArithmeticShiftRight,
};

}  // namespace lyra::hir
