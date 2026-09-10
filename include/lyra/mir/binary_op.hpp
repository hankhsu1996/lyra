#pragma once

namespace lyra::mir {

// The operators a node carries: those a target applies to two values of one
// type. An operator whose operands are not that -- a shift, whose amount is
// sized on its own -- or whose meaning is over a value's representation rather
// than over the value -- the case and wildcard equalities -- is a call against
// the entry that performs it, and reaches no node here. That holds wherever an
// operator is written, an assignment applying one included: which entry applies
// it to what a place holds is settled where the assignment is built.
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
};

}  // namespace lyra::mir
