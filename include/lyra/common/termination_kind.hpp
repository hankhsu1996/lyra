#pragma once

#include <cstdint>

namespace lyra::common {

// Termination kind values - shared between MIR, runtime, and LLVM backend.
// These values are part of the runtime ABI and must not change.
enum class TerminationKind : uint32_t {
  kFinish = 0,
  kFatal = 1,
  kStop = 2,
  kExit = 3,
};

static_assert(static_cast<uint32_t>(TerminationKind::kFinish) == 0);
static_assert(static_cast<uint32_t>(TerminationKind::kFatal) == 1);
static_assert(static_cast<uint32_t>(TerminationKind::kStop) == 2);
static_assert(static_cast<uint32_t>(TerminationKind::kExit) == 3);

}  // namespace lyra::common
