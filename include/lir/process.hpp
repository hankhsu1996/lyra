#pragma once

#include <vector>

#include "lir/instruction.hpp"

namespace volans::lir {

enum class ProcessKind { kInitial, kAlwaysComb, kAlwaysFF };

struct Process {
  ProcessKind kind;

  // Flat list of executable instructions
  std::vector<Instruction> instructions;
};

}  // namespace volans::lir
