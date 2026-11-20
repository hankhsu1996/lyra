#pragma once

#include "lyra/base/internal_error.hpp"

namespace lyra::mir {

// The operators a node carries: those a target applies to two values of one
// type, and an address moved by a machine integer's count of what it points at,
// which adding and subtracting state as C's pointer arithmetic does
// ([expr.add]). Any other operator whose operands are not that -- a shift,
// whose amount is sized on its own -- or whose meaning is over a value's
// representation rather than over the value -- the case and wildcard equalities
// -- is a call against the entry that performs it, and reaches no node here.
// That holds wherever an operator is written, an assignment applying one
// included: which entry applies it to what a place holds is settled where the
// assignment is built.
//
// Both operands are evaluated, whichever operator it is, so the logical pair
// here combines two truths already computed. An operand evaluated only where
// the other leaves the answer open is the arm of a conditional instead.
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

// Whether the operator moves an address by a count of what it points at.
constexpr auto MovesAnAddress(BinaryOp op) -> bool {
  switch (op) {
    case BinaryOp::kAdd:
    case BinaryOp::kSub:
      return true;
    case BinaryOp::kMul:
    case BinaryOp::kDiv:
    case BinaryOp::kMod:
    case BinaryOp::kBitwiseAnd:
    case BinaryOp::kBitwiseOr:
    case BinaryOp::kBitwiseXor:
    case BinaryOp::kEquality:
    case BinaryOp::kInequality:
    case BinaryOp::kGreaterEqual:
    case BinaryOp::kGreaterThan:
    case BinaryOp::kLessEqual:
    case BinaryOp::kLessThan:
    case BinaryOp::kLogicalAnd:
    case BinaryOp::kLogicalOr:
      return false;
  }
  throw InternalError("mir: unknown binary operator");
}

}  // namespace lyra::mir
