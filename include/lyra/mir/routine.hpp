#pragma once

#include <vector>

#include "lyra/common/type.hpp"
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

  // Storage metadata (for interpreter frame allocation)
  std::vector<TypeId> local_types;  // Types for each local slot
  std::vector<TypeId> temp_types;   // Types for each temp slot
};

}  // namespace lyra::mir
