#pragma once

#include <cstdint>
#include <string_view>

namespace lyra::lir {

// The operators that survive into the executable IR: those a target realizes as
// a machine instruction or a single runtime-library call. An operator's
// semantics are fixed by its operand type, not by the opcode -- an add over a
// four-state packed value propagates X, an add over a machine integer does not.
// Operators with no such realization (reductions, power, the shifts) are lifted
// to library calls before LIR and never appear here.
enum class BinaryOp : std::uint8_t {
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
  kLessThan,
  kLessEqual,
  kGreaterThan,
  kGreaterEqual,
  kLogicalAnd,
  kLogicalOr,
};

// Unary plus is an identity and is folded away before LIR. Increment and
// decrement are the successor and predecessor of a value at that value's own
// width -- the operand of a source-level `++` / `--`, whose read-modify-write
// and result ordering the lowering has already made explicit.
enum class UnaryOp : std::uint8_t {
  kMinus,
  kBitwiseNot,
  kLogicalNot,
  kIncrement,
  kDecrement,
};

// The operator's stable spelling. It names the operator in a dump and forms the
// suffix of the runtime-library entry a value domain realizes it with, so the
// two can never drift apart.
auto BinaryOpName(BinaryOp op) -> std::string_view;
auto UnaryOpName(UnaryOp op) -> std::string_view;

}  // namespace lyra::lir
