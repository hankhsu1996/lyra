#include "lyra/runtime/sim_time.hpp"

#include <cstdint>

#include "lyra/base/time.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/real.hpp"

namespace lyra::runtime {

auto TimeUnitDivisor(std::int8_t unit_power, std::int8_t global_power) noexcept
    -> SimDuration {
  SimDuration divisor = 1;
  for (int i = 0; i < unit_power - global_power; ++i) {
    divisor *= 10;
  }
  return divisor;
}

auto SimTimeInUnit(RuntimeEffects& runtime, std::int64_t unit_power)
    -> value::Time {
  const auto power = static_cast<std::int8_t>(unit_power);
  const SimDuration divisor =
      TimeUnitDivisor(power, runtime.GlobalPrecisionPower());
  const SimTime scaled = (runtime.Now() + divisor / 2) / divisor;
  return value::Time::FromInt(static_cast<std::int64_t>(scaled));
}

auto STimeInUnit(RuntimeEffects& runtime, std::int64_t unit_power)
    -> value::IntUnsigned {
  const auto power = static_cast<std::int8_t>(unit_power);
  const SimDuration divisor =
      TimeUnitDivisor(power, runtime.GlobalPrecisionPower());
  const SimTime scaled = (runtime.Now() + divisor / 2) / divisor;
  return value::IntUnsigned::FromInt(
      static_cast<std::int64_t>(static_cast<std::uint32_t>(scaled)));
}

auto RealTimeInUnit(RuntimeEffects& runtime, std::int64_t unit_power)
    -> value::Real {
  const auto power = static_cast<std::int8_t>(unit_power);
  const SimDuration divisor =
      TimeUnitDivisor(power, runtime.GlobalPrecisionPower());
  return value::Real{
      static_cast<double>(runtime.Now()) / static_cast<double>(divisor)};
}

}  // namespace lyra::runtime
