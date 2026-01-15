#pragma once

#include <vector>

#include "lyra/mir/instruction.hpp"
#include "lyra/mir/terminator.hpp"

namespace lyra::mir {

struct BasicBlock {
  BasicBlockId id;

  // Ordered, side-effecting computations
  std::vector<Instruction> instructions;

  // Exactly one terminator, always last
  Terminator terminator;
};

}  // namespace lyra::mir
