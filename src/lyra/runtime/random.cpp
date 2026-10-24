#include "lyra/runtime/random.hpp"

#include <algorithm>
#include <cstdint>

#include "lyra/runtime/distribution.hpp"
#include "lyra/runtime/rng.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/value/integral.hpp"

namespace lyra::runtime {

auto Urandom(RuntimeEffects& runtime) -> value::IntUnsigned {
  return value::IntUnsigned::FromInt(runtime.Running().rng.NextValue());
}

auto UrandomSeeded(RuntimeEffects& runtime, std::int64_t seed)
    -> value::IntUnsigned {
  DrawRng& rng = runtime.Running().rng;
  rng.Reseed(RandomSeed{static_cast<std::uint32_t>(seed)});
  return value::IntUnsigned::FromInt(rng.NextValue());
}

auto UrandomRange(
    RuntimeEffects& runtime, std::int64_t maxval, std::int64_t minval)
    -> value::IntUnsigned {
  const auto high = static_cast<std::uint32_t>(maxval);
  const auto low = static_cast<std::uint32_t>(minval);
  const std::uint32_t lower = std::min(high, low);
  const std::uint32_t upper = std::max(high, low);
  const std::uint64_t span = std::uint64_t{upper} - lower + 1;
  return value::IntUnsigned::FromInt(
      static_cast<std::uint32_t>(
          lower + DrawBelow(runtime.Running().rng, span)));
}

auto Random(RuntimeEffects& runtime) -> value::Int {
  return value::Int::FromInt(RandomDraw(runtime.UnseededRandomSeed()));
}

}  // namespace lyra::runtime
