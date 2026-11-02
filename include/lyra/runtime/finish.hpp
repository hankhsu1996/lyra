#pragma once

#include <cstdint>

#include "lyra/base/source_location.hpp"
#include "lyra/runtime/runtime_effects.hpp"

namespace lyra::runtime {

// A simulation control task (LRM 20.2) and the implicit `$finish` a `$fatal`
// makes (LRM 20.10) -- ends the run after the current slot completes, and the
// calling execution departs, so no statement after the call runs in any body,
// a function included (LRM 13.4). `$stop` suspends where `$finish` exits, and a
// run nothing can resume tells the two apart only in what it prints, so the
// task's own name is what reaches the engine, and `at` is where the call is
// written.
[[noreturn]] void Finish(
    RuntimeEffects& runtime, std::int64_t finish_number,
    base::SourceLocation at);

[[noreturn]] void Stop(
    RuntimeEffects& runtime, std::int64_t finish_number,
    base::SourceLocation at);

}  // namespace lyra::runtime
