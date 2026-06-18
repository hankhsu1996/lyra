#pragma once

#include <cstdint>

#include "lyra/base/time.hpp"
#include "lyra/runtime/runtime_services.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::runtime {

// The factor that converts the design-global tick (LRM 3.14.3) to one step of a
// scope whose time unit is `unit_power`: 10^(unit_power - global_power). The
// exponent is non-negative because the global precision is the finest in the
// design, so a scope unit is never finer than the tick. The inverse of
// `ScaleToGlobalTicks` (which a delay uses to go the other way).
inline auto TimeUnitDivisor(
    std::int8_t unit_power, std::int8_t global_power) noexcept -> SimDuration {
  SimDuration divisor = 1;
  for (int i = 0; i < unit_power - global_power; ++i) {
    divisor *= 10;
  }
  return divisor;
}

// $time (LRM 20.3.1): the current time scaled to `unit_power` and rounded to
// the nearest integer (only the unit conversion rounds; precision does not).
// `unit_power` arrives as a Lyra value, the same as any other call argument.
inline auto SimTimeInUnit(
    RuntimeServices& services, const value::PackedArray& unit_power)
    -> value::PackedArray {
  const auto power = static_cast<std::int8_t>(unit_power.ToInt64());
  const SimDuration divisor =
      TimeUnitDivisor(power, services.GlobalPrecisionPower());
  const SimTime scaled = (services.Now() + divisor / 2) / divisor;
  return value::PackedArray::FromInt(
      static_cast<std::int64_t>(scaled), 64, false, true);
}

// $stime (LRM 20.3.2): the low 32 bits of the $time value.
inline auto STimeInUnit(
    RuntimeServices& services, const value::PackedArray& unit_power)
    -> value::PackedArray {
  const auto power = static_cast<std::int8_t>(unit_power.ToInt64());
  const SimDuration divisor =
      TimeUnitDivisor(power, services.GlobalPrecisionPower());
  const SimTime scaled = (services.Now() + divisor / 2) / divisor;
  return value::PackedArray::FromInt(
      static_cast<std::int64_t>(static_cast<std::uint32_t>(scaled)), 32, true,
      false);
}

// $realtime (LRM 20.3.3): the current time scaled to `unit_power` as a real,
// keeping any fractional part.
inline auto RealTimeInUnit(
    RuntimeServices& services, const value::PackedArray& unit_power) -> double {
  const auto power = static_cast<std::int8_t>(unit_power.ToInt64());
  const SimDuration divisor =
      TimeUnitDivisor(power, services.GlobalPrecisionPower());
  return static_cast<double>(services.Now()) / static_cast<double>(divisor);
}

}  // namespace lyra::runtime
