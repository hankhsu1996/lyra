#pragma once

#include <vector>

#include "lyra/mir/handle.hpp"

namespace lyra::mir {

struct Terminator {
  enum class Kind {
    // Control
    kJump,
    kBranch,
    kSwitch,

    // Suspension
    kDelay,
    kWait,

    // Completion
    kReturn,
    kFinish,
    kRepeat,
  };

  Kind kind;

  // Generic fields; exact payloads can be refined later
  std::vector<BasicBlockId> targets;  // successors or resume targets
  int condition_operand;              // for branch/switch (opaque handle)
};

}  // namespace lyra::mir
