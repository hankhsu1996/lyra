#pragma once

#include <cstdint>

#include "lyra/base/time.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/real.hpp"

namespace lyra::runtime {

// `value` times 10^`exponent`, for an exponent of zero or more, capping at the
// widest duration rather than wrapping. The cap is not defensive: LRM 9.4.1
// gives a negative delay the value of its own bits read as an unsigned
// integer, which is a wait no simulation reaches, and wrapping would turn
// exactly that into a short one.
auto MultiplyByPowerOfTen(SimDuration value, int exponent) noexcept
    -> SimDuration;

// `steps` of 10^`unit_power` seconds as ticks of the global time precision (LRM
// 3.14.3), and `ticks` as such steps: exact where the step is the coarser of
// the two, and rounded to the nearest, a half up, where it is the finer. A
// step is never finer than the tick in a scope that states its precision,
// since the tick is the smallest of those; one left to the default time unit
// (LRM 3.14.2.3) may be finer.
auto StepsToTicks(
    SimDuration steps, std::int8_t unit_power,
    std::int8_t global_power) noexcept -> SimDuration;
auto TicksToSteps(
    SimTime ticks, std::int8_t unit_power, std::int8_t global_power) noexcept
    -> SimTime;

// The current time scaled to `unit_power` and rounded to the nearest integer
// (only the unit conversion rounds; precision does not).
auto NowInUnit(RuntimeEffects& runtime, std::int8_t unit_power) -> SimTime;

// $time (LRM 20.3.1): the current time in `unit_power`, rounded to the
// nearest integer.
auto SimTimeInUnit(RuntimeEffects& runtime, std::int64_t unit_power)
    -> value::Time;

// $stime (LRM 20.3.2): the low 32 bits of the $time value, as an unsigned
// integer.
auto STimeInUnit(RuntimeEffects& runtime, std::int64_t unit_power)
    -> value::IntUnsigned;

// $realtime (LRM 20.3.3): the current time scaled to `unit_power` as a real,
// keeping any fractional part.
auto RealTimeInUnit(RuntimeEffects& runtime, std::int64_t unit_power)
    -> value::Real;

}  // namespace lyra::runtime
