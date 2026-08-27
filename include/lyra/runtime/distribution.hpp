#pragma once

#include <algorithm>
#include <bit>
#include <cmath>
#include <cstdint>
#include <format>
#include <limits>
#include <string_view>

#include "lyra/base/simulation_error.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/tuple.hpp"

namespace lyra::runtime {

// What one LRM 20.14 call hands back: the number it drew, and the seed that
// draw left behind. The seed is an `inout` argument (LRM 20.14.2), so the
// advanced seed is a second value, which the caller stores back into the
// variable the design keeps its stream in.
using DistributionDraw = value::Tuple<value::PackedArray, value::PackedArray>;

namespace detail {

// LRM Annex N gives these functions as C source and reads its `long` as 32 bits
// wide. What forces that reading is the branch `$random` takes -- Table N.1
// defines `$random` as `rtl_dist_uniform(seed, LONG_MIN, LONG_MAX)` -- which
// rescales by 4294967295 and 4294967296 and so answers within 32 signed bits
// only when the bounds it compares against are the 32-bit ones.
using DistInt = std::int32_t;

constexpr DistInt kDistIntMax = std::numeric_limits<DistInt>::max();
constexpr DistInt kDistIntMin = std::numeric_limits<DistInt>::min();

// The generator underneath every function here, and the only step that moves
// the seed (LRM Annex N). It advances a linear congruential state, reads the
// state's low bits as the mantissa of a float in [1, 2), and rescales that onto
// the requested interval; bounds that describe no interval are replaced by the
// full non-negative one.
inline auto Uniform(DistInt& seed, DistInt start, DistInt end) -> double {
  constexpr double kMantissaStep = 0.00000011920928955078125;

  if (seed == 0) {
    seed = 259341593;
  }

  double low = 0.0;
  double high = 2147483647.0;
  if (start < end) {
    low = static_cast<double>(start);
    high = static_cast<double>(end);
  }

  seed = static_cast<DistInt>((69069U * static_cast<std::uint32_t>(seed)) + 1U);

  const auto mantissa = (static_cast<std::uint32_t>(seed) >> 9U) | 0x3f800000U;
  auto scaled = static_cast<double>(std::bit_cast<float>(mantissa));

  scaled = scaled + (scaled * kMantissaStep);
  return ((high - low) * (scaled - 1.0)) + low;
}

// A Box-Muller polar draw: rejection-sample a point inside the unit circle,
// then shape one of its coordinates by the requested mean and deviation.
inline auto Normal(DistInt& seed, DistInt mean, DistInt deviation) -> double {
  double first = 0.0;
  double second = 0.0;
  double radius = 1.0;
  while ((radius >= 1.0) || (radius == 0.0)) {
    first = Uniform(seed, -1, 1);
    second = Uniform(seed, -1, 1);
    radius = (first * first) + (second * second);
  }
  const double shaped = first * std::sqrt(-2.0 * std::log(radius) / radius);
  return (shaped * static_cast<double>(deviation)) + static_cast<double>(mean);
}

// Inverse transform sampling: the negated log of a uniform draw on [0, 1),
// scaled to the requested mean.
inline auto Exponential(DistInt& seed, DistInt mean) -> double {
  const double draw = Uniform(seed, 0, 1);
  if (draw == 0) {
    return draw;
  }
  return -std::log(draw) * static_cast<double>(mean);
}

// Knuth's method: multiply uniform draws until their running product falls
// below the mean's negative exponential, and count how many that took.
inline auto Poisson(DistInt& seed, DistInt mean) -> DistInt {
  DistInt count = 0;
  const double limit = std::exp(-static_cast<double>(mean));
  double product = Uniform(seed, 0, 1);
  while (limit < product) {
    ++count;
    product = Uniform(seed, 0, 1) * product;
  }
  return count;
}

// A sum of squared standard normals, one per degree of freedom, with each pair
// of degrees taken as an exponential draw instead.
inline auto ChiSquare(DistInt& seed, DistInt degrees_of_freedom) -> double {
  double total = 0.0;
  if (degrees_of_freedom % 2 != 0) {
    total = Normal(seed, 0, 1);
    total = total * total;
  }
  for (DistInt k = 2; k <= degrees_of_freedom; k = k + 2) {
    total = total + (2 * Exponential(seed, 1));
  }
  return total;
}

// A standard normal draw divided by the root of a chi-square draw over its own
// degrees of freedom, which is the ratio Student's t is defined as.
inline auto StudentT(DistInt& seed, DistInt degrees_of_freedom) -> double {
  const double chi2 = ChiSquare(seed, degrees_of_freedom);
  const double root = std::sqrt(chi2 / static_cast<double>(degrees_of_freedom));
  return Normal(seed, 0, 1) / root;
}

// A sum of k exponential stages, taken as the log of their uniform product.
inline auto Erlangian(DistInt& seed, DistInt stages, DistInt mean) -> double {
  double product = 1.0;
  for (DistInt i = 1; i <= stages; ++i) {
    product = product * Uniform(seed, 0, 1);
  }
  return -static_cast<double>(mean) * std::log(product) /
         static_cast<double>(stages);
}

// LRM Annex N rounds a real-valued draw to the nearest integer, away from zero
// at a half.
inline auto RoundDraw(double draw) -> DistInt {
  const double magnitude = std::floor(std::abs(draw) + 0.5);
  return static_cast<DistInt>(draw >= 0 ? magnitude : -magnitude);
}

// The integer reading LRM Annex N applies to a draw. It is neither truncation
// nor a floor -- a negative draw is stepped down before it is truncated, so a
// negative whole number reads one lower than either would give -- and neither
// `std::trunc` nor `std::floor` is a substitute for it.
inline auto TruncateDraw(double draw) -> DistInt {
  return static_cast<DistInt>(draw >= 0 ? draw : draw - 1);
}

// LRM Annex N: a value in the closed interval the bounds describe, reached by
// widening the interval by one and truncating a uniform draw into it. The
// full-width interval is `$random`'s, and is the one that cannot widen, so it
// rescales the draw across all 32 signed bits instead.
inline auto UniformIn(DistInt& seed, DistInt start, DistInt end) -> DistInt {
  if (start >= end) {
    return start;
  }

  if (end != kDistIntMax) {
    ++end;
    const DistInt drawn = TruncateDraw(Uniform(seed, start, end));
    return std::clamp(drawn, start, static_cast<DistInt>(end - 1));
  }

  if (start != kDistIntMin) {
    --start;
    const DistInt drawn = TruncateDraw(Uniform(seed, start, end) + 1.0);
    return std::clamp(drawn, static_cast<DistInt>(start + 1), end);
  }

  const double scaled =
      (Uniform(seed, start, end) + 2147483648.0) / 4294967295.0;
  return TruncateDraw((scaled * 4294967296.0) - 2147483648.0);
}

// Every argument to these functions is an integer value (LRM 20.14.2),
// whatever integral type the design declared the variable it came from.
inline auto IntegerArgument(const value::PackedArray& value) -> DistInt {
  return static_cast<DistInt>(value.ToInt64());
}

inline auto DrawnWith(DistInt value, DistInt seed) -> DistributionDraw {
  return DistributionDraw{
      value::PackedArray::Int(value), value::PackedArray::Int(seed)};
}

// LRM 20.14.2 requires a positive mean, degree of freedom, and stage count. A
// design that asks for anything else has asked for a distribution that does not
// exist, which is its own failure rather than a limit of this implementation.
inline void RequirePositive(
    DistInt shape, std::string_view subroutine, std::string_view argument) {
  if (shape <= 0) {
    throw SimulationError(
        std::format(
            "{} requires a positive {} (LRM 20.14.2)", subroutine, argument));
  }
}

}  // namespace detail

// $dist_uniform (LRM 20.14.2): uniformly distributed over the closed interval
// the two bounds describe.
inline auto DistUniform(
    const value::PackedArray& seed, const value::PackedArray& start,
    const value::PackedArray& end) -> DistributionDraw {
  detail::DistInt state = detail::IntegerArgument(seed);
  const detail::DistInt drawn = detail::UniformIn(
      state, detail::IntegerArgument(start), detail::IntegerArgument(end));
  return detail::DrawnWith(drawn, state);
}

// $dist_normal (LRM 20.14.2): the standard deviation widens the spread and is
// the one shape argument the standard leaves unconstrained.
inline auto DistNormal(
    const value::PackedArray& seed, const value::PackedArray& mean,
    const value::PackedArray& standard_deviation) -> DistributionDraw {
  detail::DistInt state = detail::IntegerArgument(seed);
  const double drawn = detail::Normal(
      state, detail::IntegerArgument(mean),
      detail::IntegerArgument(standard_deviation));
  return detail::DrawnWith(detail::RoundDraw(drawn), state);
}

// $dist_exponential (LRM 20.14.2): the mean is what the values drawn average
// towards.
inline auto DistExponential(
    const value::PackedArray& seed, const value::PackedArray& mean)
    -> DistributionDraw {
  const detail::DistInt shape = detail::IntegerArgument(mean);
  detail::RequirePositive(shape, "$dist_exponential", "mean");
  detail::DistInt state = detail::IntegerArgument(seed);
  const double drawn = detail::Exponential(state, shape);
  return detail::DrawnWith(detail::RoundDraw(drawn), state);
}

// $dist_poisson (LRM 20.14.2). The only one of the family whose generator
// already answers with a count, so nothing is rounded.
inline auto DistPoisson(
    const value::PackedArray& seed, const value::PackedArray& mean)
    -> DistributionDraw {
  const detail::DistInt shape = detail::IntegerArgument(mean);
  detail::RequirePositive(shape, "$dist_poisson", "mean");
  detail::DistInt state = detail::IntegerArgument(seed);
  const detail::DistInt drawn = detail::Poisson(state, shape);
  return detail::DrawnWith(drawn, state);
}

// $dist_chi_square (LRM 20.14.2): the degree of freedom shapes the density,
// and a larger one spreads the values wider.
inline auto DistChiSquare(
    const value::PackedArray& seed,
    const value::PackedArray& degrees_of_freedom) -> DistributionDraw {
  const detail::DistInt shape = detail::IntegerArgument(degrees_of_freedom);
  detail::RequirePositive(shape, "$dist_chi_square", "degree of freedom");
  detail::DistInt state = detail::IntegerArgument(seed);
  const double drawn = detail::ChiSquare(state, shape);
  return detail::DrawnWith(detail::RoundDraw(drawn), state);
}

// $dist_t (LRM 20.14.2): the degree of freedom shapes the density, and a
// larger one spreads the values wider.
inline auto DistT(
    const value::PackedArray& seed,
    const value::PackedArray& degrees_of_freedom) -> DistributionDraw {
  const detail::DistInt shape = detail::IntegerArgument(degrees_of_freedom);
  detail::RequirePositive(shape, "$dist_t", "degree of freedom");
  detail::DistInt state = detail::IntegerArgument(seed);
  const double drawn = detail::StudentT(state, shape);
  return detail::DrawnWith(detail::RoundDraw(drawn), state);
}

// $dist_erlang (LRM 20.14.2): a k-stage distribution, the mean being what the
// values drawn average towards.
inline auto DistErlang(
    const value::PackedArray& seed, const value::PackedArray& stages,
    const value::PackedArray& mean) -> DistributionDraw {
  const detail::DistInt stage_count = detail::IntegerArgument(stages);
  const detail::DistInt average = detail::IntegerArgument(mean);
  detail::RequirePositive(stage_count, "$dist_erlang", "k_stage");
  detail::RequirePositive(average, "$dist_erlang", "mean");
  detail::DistInt state = detail::IntegerArgument(seed);
  const double drawn = detail::Erlangian(state, stage_count, average);
  return detail::DrawnWith(detail::RoundDraw(drawn), state);
}

}  // namespace lyra::runtime
