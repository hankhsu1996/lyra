#pragma once

#include <cstdint>

#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/value/integral.hpp"

namespace lyra::runtime {

// $urandom (LRM 18.13.1).
auto Urandom(RuntimeEffects& runtime) -> value::IntUnsigned;

// $urandom with a seed (LRM 18.13.1): the seed determines the sequence, so it
// restarts the generator this call draws from before drawing, and the same seed
// replays the same values.
auto UrandomSeeded(RuntimeEffects& runtime, std::int64_t seed)
    -> value::IntUnsigned;

// $urandom_range (LRM 18.13.2): a value in the closed range the two bounds
// describe. The bounds are unsigned and arrive in either order -- the standard
// reverses them when the high one is the smaller -- so the span is taken from
// the ordered pair rather than from the arguments' positions.
auto UrandomRange(
    RuntimeEffects& runtime, std::int64_t maxval, std::int64_t minval)
    -> value::IntUnsigned;

// $random called with no seed (LRM 20.14.1). The algorithm is the standard's
// whether or not the call names a seed (LRM 20.14, Annex N), so this is the
// seeded draw over a seed the run keeps. The run keeps one because the values
// are meant to be the same in every implementation (LRM 20.14), which a start
// taken from a process's own generator would not be, and because LRM 18.14
// does not list `$random` among what random stability covers.
auto Random(RuntimeEffects& runtime) -> value::Int;

}  // namespace lyra::runtime
