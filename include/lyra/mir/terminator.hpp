#pragma once

#include <cstdint>
#include <optional>
#include <vector>

#include "lyra/mir/handle.hpp"

namespace lyra::mir {

enum class TerminationKind : uint8_t {
  kFinish,  // normal termination ($finish)
  kFatal,   // error termination ($fatal - non-zero exit)
  kStop,    // pause for debugger ($stop)
  kExit,    // normal termination ($exit - synonym for $finish)
};

struct TerminationInfo {
  TerminationKind kind;
  int level;  // 0 = silent, 1 = print time, 2 = print time+stats
};

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

  // Only valid for kFinish terminator
  std::optional<TerminationInfo> termination_info;
};

}  // namespace lyra::mir
