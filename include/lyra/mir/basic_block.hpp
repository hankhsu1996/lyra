#pragma once

#include <vector>

#include "lyra/mir/statement.hpp"
#include "lyra/mir/terminator.hpp"

namespace lyra::mir {

struct BasicBlock {
  // Ordered, side-effecting statements (no control flow)
  std::vector<Statement> statements;

  // Exactly one terminator, always last
  Terminator terminator;
};

}  // namespace lyra::mir
