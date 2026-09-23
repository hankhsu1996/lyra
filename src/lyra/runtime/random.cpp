#include "lyra/runtime/random.hpp"

#include <algorithm>
#include <cstdint>

#include "lyra/runtime/rng.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::runtime {

auto Urandom(RuntimeEffects& runtime) -> value::PackedArray {
  return value::PackedArray::IntUnsigned(runtime.Running().rng.NextValue());
}

auto UrandomSeeded(RuntimeEffects& runtime, const value::PackedArray& seed)
    -> value::PackedArray {
  DrawRng& rng = runtime.Running().rng;
  rng.Reseed(RandomSeed{static_cast<std::uint32_t>(seed.ToInt64())});
  return value::PackedArray::IntUnsigned(rng.NextValue());
}

auto UrandomRange(
    RuntimeEffects& runtime, const value::PackedArray& maxval,
    const value::PackedArray& minval) -> value::PackedArray {
  const auto high = static_cast<std::uint32_t>(maxval.ToInt64());
  const auto low = static_cast<std::uint32_t>(minval.ToInt64());
  const std::uint32_t lower = std::min(high, low);
  const std::uint32_t upper = std::max(high, low);
  const std::uint64_t span = std::uint64_t{upper} - lower + 1;
  DrawRng& rng = runtime.Running().rng;
  // Rejection rather than a modulo of the raw draw: the low values would
  // otherwise come up more often whenever the span does not divide the
  // generator's range, which is every span that is not a power of two. A span
  // covering the whole range divides it exactly, so nothing is ever rejected.
  const std::uint64_t limit = (std::uint64_t{1} << 32U) / span * span;
  std::uint64_t draw = rng.NextValue();
  while (draw >= limit) {
    draw = rng.NextValue();
  }
  return value::PackedArray::IntUnsigned(
      static_cast<std::uint32_t>(lower + (draw % span)));
}

auto Random(RuntimeEffects& runtime) -> value::PackedArray {
  return value::PackedArray::Int(
      static_cast<std::int32_t>(runtime.Running().rng.NextValue()));
}

}  // namespace lyra::runtime
