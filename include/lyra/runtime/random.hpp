#pragma once

#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::runtime {

// $urandom (LRM 18.13.1).
auto Urandom(RuntimeEffects& runtime) -> value::PackedArray;

// $urandom with a seed (LRM 18.13.1): the seed determines the sequence, so it
// restarts the generator this call draws from before drawing, and the same seed
// replays the same values.
auto UrandomSeeded(RuntimeEffects& runtime, const value::PackedArray& seed)
    -> value::PackedArray;

// $urandom_range (LRM 18.13.2): a value in the closed range the two bounds
// describe. The bounds are unsigned and arrive in either order -- the standard
// reverses them when the high one is the smaller -- so the span is taken from
// the ordered pair rather than from the arguments' positions.
auto UrandomRange(
    RuntimeEffects& runtime, const value::PackedArray& maxval,
    const value::PackedArray& minval) -> value::PackedArray;

// $random called with no seed (LRM 20.14.1). The standard gives the seeded form
// a generator of its own and states no source for the bits when the call
// carries no seed, and LRM 18.14 does not list `$random` among what random
// stability covers, so nothing fixes where an unseeded draw comes from. It
// comes from the generator the call draws from, which makes it a signed
// reading of the same 32 bits `$urandom` answers with and gives it the same
// locality.
auto Random(RuntimeEffects& runtime) -> value::PackedArray;

}  // namespace lyra::runtime
