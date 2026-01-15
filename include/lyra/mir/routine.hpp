#pragma once

#include <vector>

#include "lyra/mir/handle.hpp"

namespace lyra::mir {

enum class ProcessKind {
  kOnce,     // initial block - runs once
  kLooping,  // always block - repeats
};

// A process is a coroutine unit that may suspend and resume.
// Allowed terminators: Control, Suspension (Delay/Wait), Completion
// (Finish/Repeat).
struct Process {
  ProcessKind kind = ProcessKind::kOnce;
  BasicBlockId entry;
  std::vector<BasicBlockId> blocks;
};

// A function is a callable unit that cannot suspend.
// Allowed terminators: Control (Jump/Branch/Switch), Return.
struct Function {
  BasicBlockId entry;
  std::vector<BasicBlockId> blocks;
};

}  // namespace lyra::mir
