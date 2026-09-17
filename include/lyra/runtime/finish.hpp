#pragma once

#include <string_view>

#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/string.hpp"

namespace lyra::runtime {

// A simulation control task (LRM 20.2) and the implicit `$finish` a `$fatal`
// makes (LRM 20.10) -- ends the run after the current slot completes. The
// calling execution gives up control and is never dispatched again, which is
// the one departure that waits for nothing: the run is over, so there is
// nothing for it to be waiting for and nothing that could restart it.
// `origin` and `level` arrive as Lyra values, the same as any other call
// argument. `$stop` suspends where `$finish` exits, and a run nothing can
// resume tells the two apart only in what it prints, so the task's own name is
// what reaches the engine.
inline auto EndRunFrom(
    RuntimeEffects& runtime, std::string_view task,
    const lyra::value::String& origin, const lyra::value::PackedArray& level)
    -> bool {
  runtime.EndRun(task, origin, level);
  return true;
}

inline auto Finish(
    RuntimeEffects& runtime, const lyra::value::String& origin,
    const lyra::value::PackedArray& level) -> bool {
  return EndRunFrom(runtime, "$finish", origin, level);
}

inline auto Stop(
    RuntimeEffects& runtime, const lyra::value::String& origin,
    const lyra::value::PackedArray& level) -> bool {
  return EndRunFrom(runtime, "$stop", origin, level);
}

}  // namespace lyra::runtime
