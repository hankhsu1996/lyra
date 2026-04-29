#pragma once

namespace lyra::mir {

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

}  // namespace lyra::mir
