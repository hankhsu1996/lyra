#include "lyra/runtime/finish.hpp"

#include <string_view>

#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/string.hpp"

namespace lyra::runtime {

namespace {

// The two tasks differ only in the name the engine is told, so what each of
// them does is written once.
auto EndRunFrom(
    RuntimeEffects& runtime, std::string_view task,
    const lyra::value::String& origin, const lyra::value::PackedArray& level)
    -> bool {
  runtime.EndRun(task, origin, level);
  return true;
}

}  // namespace

auto Finish(
    RuntimeEffects& runtime, const lyra::value::String& origin,
    const lyra::value::PackedArray& level) -> bool {
  return EndRunFrom(runtime, "$finish", origin, level);
}

auto Stop(
    RuntimeEffects& runtime, const lyra::value::String& origin,
    const lyra::value::PackedArray& level) -> bool {
  return EndRunFrom(runtime, "$stop", origin, level);
}

}  // namespace lyra::runtime
