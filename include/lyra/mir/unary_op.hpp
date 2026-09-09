#pragma once

namespace lyra::mir {

// The operators a unary node carries: those a target language applies to a
// value directly. An operator whose meaning is an operation over the operand's
// parts is a call against the entry that performs it, and one that is the
// identity is its operand, so neither reaches a node here.
enum class UnaryOp {
  kMinus,
  kBitwiseNot,
  kLogicalNot,
};

}  // namespace lyra::mir
