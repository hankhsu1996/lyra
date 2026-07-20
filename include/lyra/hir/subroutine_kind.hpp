#pragma once

#include <cstdint>

namespace lyra::hir {

// LRM 13.1: a subroutine is a function or a task. The distinction is the call
// protocol -- a task enable may consume time and suspends its caller until
// completion (LRM 13.3), a function completes in zero time -- so it is read at
// the call site to decide whether the enable awaits.
enum class SubroutineKind : std::uint8_t {
  kTask,
  kFunction,
};

}  // namespace lyra::hir
