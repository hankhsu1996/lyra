#pragma once

namespace lyra::hir {

enum class UnaryOp {
  kPlus,
  kMinus,
  kBitwiseNot,
  kLogicalNot,
  kReductionAnd,
  kReductionOr,
  kReductionXor,
  kReductionNand,
  kReductionNor,
  kReductionXnor,
};

}  // namespace lyra::hir
