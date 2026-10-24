#pragma once

#include <cstdint>

namespace lyra::support {

// Where a `disable` of a target lands in an execution inside it (LRM 9.6.2).
// Shared vocabulary: the compiler states it where an execution enters the
// target, and the runtime reads it to decide whether a `disable` resumes that
// execution or ends it.
enum class TargetLanding : std::uint8_t {
  // At the statement after the target, where the execution continues.
  kPastTheTarget,
  // At the execution's end: the target is the whole of what the execution
  // runs -- its own statement, or the target it was spawned within -- so
  // nothing of it follows the target, and a `disable` of it ends the execution
  // (LRM 9.6.3).
  kAtExecutionEnd,
};

}  // namespace lyra::support
