#pragma once

#include <cstdint>

#include "lyra/base/time.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/real.hpp"

namespace lyra::runtime {

// The factor that converts the design-global tick (LRM 3.14.3) to one step of a
// scope whose time unit is `unit_power`: 10^(unit_power - global_power). The
// exponent is non-negative because the global precision is the finest in the
// design, so a scope unit is never finer than the tick. It is the inverse of
// the scaling a delay applies, which goes from a scope's steps to the tick.
auto TimeUnitDivisor(std::int8_t unit_power, std::int8_t global_power) noexcept
    -> SimDuration;

// $time (LRM 20.3.1): the current time scaled to `unit_power` and rounded to
// the nearest integer (only the unit conversion rounds; precision does not).
// `unit_power` arrives as a Lyra value, the same as any other call argument.
auto SimTimeInUnit(
    RuntimeEffects& runtime, const value::PackedArray& unit_power)
    -> value::PackedArray;

// $stime (LRM 20.3.2): the low 32 bits of the $time value.
auto STimeInUnit(RuntimeEffects& runtime, const value::PackedArray& unit_power)
    -> value::PackedArray;

// $realtime (LRM 20.3.3): the current time scaled to `unit_power` as a real,
// keeping any fractional part.
auto RealTimeInUnit(
    RuntimeEffects& runtime, const value::PackedArray& unit_power)
    -> value::Real;

}  // namespace lyra::runtime
