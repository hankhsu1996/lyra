#pragma once

#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/string.hpp"

namespace lyra::runtime {

// A simulation control task (LRM 20.2) and the implicit `$finish` a `$fatal`
// makes (LRM 20.10) -- ends the run after the current slot completes, and the
// calling execution departs, so no statement after the call runs in any body,
// a function included (LRM 13.4). `origin` and `level` arrive as Lyra values,
// the same as any other call argument. `$stop` suspends where `$finish` exits,
// and a run nothing can resume tells the two apart only in what it prints, so
// the task's own name is what reaches the engine.
[[noreturn]] void Finish(
    RuntimeEffects& runtime, const lyra::value::String& origin,
    const lyra::value::PackedArray& level);

[[noreturn]] void Stop(
    RuntimeEffects& runtime, const lyra::value::String& origin,
    const lyra::value::PackedArray& level);

}  // namespace lyra::runtime
