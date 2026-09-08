#pragma once

#include <optional>

#include "lyra/support/builtin_fn.hpp"

namespace lyra::mir {

// The operators a node carries: those a target applies to two values of one
// type. An operator whose operands are not that -- a shift, whose amount is
// sized on its own -- or whose meaning is over a value's representation rather
// than over the value -- the case and wildcard equalities -- is a call against
// the entry that performs it, and reaches no node here.
//
// A shift is the exception that stays: a compound assignment names its operator
// rather than the operation, so it needs one for `<<=` even though no
// expression node ever carries one.
enum class BinaryOp {
  kAdd,
  kSub,
  kMul,
  kDiv,
  kMod,
  kBitwiseAnd,
  kBitwiseOr,
  kBitwiseXor,
  kEquality,
  kInequality,
  kGreaterEqual,
  kGreaterThan,
  kLessEqual,
  kLessThan,
  kLogicalAnd,
  kLogicalOr,
  kShiftLeft,
  kLogicalShiftRight,
  kArithmeticShiftRight,
};

// The runtime-library entry an operator is performed through, or nullopt for
// one a target applies directly. Only the shifts answer with an entry, and only
// a compound assignment can present one, because a shift's operands are sized
// separately and a two-operand form of one type cannot say that. One source for
// every consumer of a compound assignment, which must agree on it.
auto BinaryOpAsBuiltinFn(BinaryOp op) -> std::optional<support::BuiltinFn>;

}  // namespace lyra::mir
