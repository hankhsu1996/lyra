#pragma once

#include "lyra/value/packed_array.hpp"
#include "lyra/value/tuple.hpp"

namespace lyra::runtime {

// What one LRM 20.14 call hands back: the number it drew, and the seed that
// draw left behind. The seed is an `inout` argument (LRM 20.14.2), so the
// advanced seed is a second value, which the caller stores back into the
// variable the design keeps its stream in.
using DistributionDraw = value::Tuple<value::PackedArray, value::PackedArray>;

// $dist_uniform (LRM 20.14.2): uniformly distributed over the closed interval
// the two bounds describe.
auto DistUniform(
    const value::PackedArray& seed, const value::PackedArray& start,
    const value::PackedArray& end) -> DistributionDraw;

// $dist_normal (LRM 20.14.2): the standard deviation widens the spread and is
// the one shape argument the standard leaves unconstrained.
auto DistNormal(
    const value::PackedArray& seed, const value::PackedArray& mean,
    const value::PackedArray& standard_deviation) -> DistributionDraw;

// $dist_exponential (LRM 20.14.2): the mean is what the values drawn average
// towards.
auto DistExponential(
    const value::PackedArray& seed, const value::PackedArray& mean)
    -> DistributionDraw;

// $dist_poisson (LRM 20.14.2). The only one of the family whose generator
// already answers with a count, so nothing is rounded.
auto DistPoisson(const value::PackedArray& seed, const value::PackedArray& mean)
    -> DistributionDraw;

// $dist_chi_square (LRM 20.14.2): the degree of freedom shapes the density,
// and a larger one spreads the values wider.
auto DistChiSquare(
    const value::PackedArray& seed,
    const value::PackedArray& degrees_of_freedom) -> DistributionDraw;

// $dist_t (LRM 20.14.2): the degree of freedom shapes the density, and a
// larger one spreads the values wider.
auto DistT(
    const value::PackedArray& seed,
    const value::PackedArray& degrees_of_freedom) -> DistributionDraw;

// $dist_erlang (LRM 20.14.2): a k-stage distribution, the mean being what the
// values drawn average towards.
auto DistErlang(
    const value::PackedArray& seed, const value::PackedArray& stages,
    const value::PackedArray& mean) -> DistributionDraw;

}  // namespace lyra::runtime
