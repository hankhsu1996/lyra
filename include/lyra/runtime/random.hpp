#pragma once

#include <algorithm>
#include <cstdint>

#include "lyra/base/simulation_error.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::runtime {

namespace detail {

// The generator the calling process draws from (LRM 18.14.2). A static
// variable's initializer runs ahead of every procedure (LRM 6.8), so a legal
// program can reach a randomization call with no process executing; that is a
// generator this implementation does not provide yet rather than a violated
// invariant.
inline auto DrawingProcessRng(RuntimeEffects& runtime) -> ProcessRng& {
  RuntimeProcess* process = runtime.TryCurrentProcess();
  if (process == nullptr) {
    throw SimulationError(
        "a random number function called outside any process is not yet "
        "supported");
  }
  return process->Rng();
}

}  // namespace detail

// $urandom (LRM 18.13.1).
inline auto Urandom(RuntimeEffects& runtime) -> value::PackedArray {
  return value::PackedArray::IntUnsigned(
      detail::DrawingProcessRng(runtime).NextValue());
}

// $urandom with a seed (LRM 18.13.1): the seed determines the sequence, so it
// restarts the calling process's generator before the draw and the same seed
// replays the same values.
inline auto UrandomSeeded(
    RuntimeEffects& runtime, const value::PackedArray& seed)
    -> value::PackedArray {
  ProcessRng& rng = detail::DrawingProcessRng(runtime);
  rng.Reseed(RandomSeed{static_cast<std::uint32_t>(seed.ToInt64())});
  return value::PackedArray::IntUnsigned(rng.NextValue());
}

// $urandom_range (LRM 18.13.2): a value in the closed range the two bounds
// describe. The bounds are unsigned and arrive in either order -- the standard
// reverses them when the high one is the smaller -- so the span is taken from
// the ordered pair rather than from the arguments' positions.
inline auto UrandomRange(
    RuntimeEffects& runtime, const value::PackedArray& maxval,
    const value::PackedArray& minval) -> value::PackedArray {
  const auto high = static_cast<std::uint32_t>(maxval.ToInt64());
  const auto low = static_cast<std::uint32_t>(minval.ToInt64());
  const std::uint32_t lower = std::min(high, low);
  const std::uint32_t upper = std::max(high, low);
  const std::uint64_t span = std::uint64_t{upper} - lower + 1;
  ProcessRng& rng = detail::DrawingProcessRng(runtime);
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

// $random called with no seed (LRM 20.14.1). The standard gives the seeded form
// a generator of its own and states no source for the bits when the call
// carries no seed, and LRM 18.14 does not list `$random` among what random
// stability covers, so nothing fixes where an unseeded draw comes from. It
// comes from the calling process, which makes it a signed reading of the same
// 32 bits `$urandom` answers with, and gives it that call's thread locality.
inline auto Random(RuntimeEffects& runtime) -> value::PackedArray {
  return value::PackedArray::Int(
      static_cast<std::int32_t>(
          detail::DrawingProcessRng(runtime).NextValue()));
}

}  // namespace lyra::runtime
