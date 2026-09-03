#pragma once

#include <optional>

#include "lyra/support/builtin_fn.hpp"

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
  kCasezEquality,
  kCasexEquality,
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

// The runtime-library entry a binary operator is realized through, or nullopt
// for an operator a backend realizes directly as a binary operation. The ones
// with an entry -- the shifts, power, the case and wildcard equalities -- carry
// width and signedness rules that a plain two-operand form cannot express, so
// they cross as a call whatever consumes them. One source for both the
// expression path and the compound-assignment path, which must agree.
auto BinaryOpAsBuiltinFn(BinaryOp op) -> std::optional<support::BuiltinFn>;

}  // namespace lyra::mir
