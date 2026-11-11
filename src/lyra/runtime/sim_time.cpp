#include "lyra/runtime/sim_time.hpp"

#include <cstdint>
#include <limits>

#include "lyra/base/time.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/real.hpp"

namespace lyra::runtime {

namespace {

// `value` divided by 10^`exponent`, for an exponent of zero or more, rounded
// to the nearest integer, a half up.
auto DivideByPowerOfTen(SimDuration value, int exponent) noexcept
    -> SimDuration {
  SimDuration divisor = 1;
  for (int i = 0; i < exponent; ++i) {
    if (divisor > std::numeric_limits<SimDuration>::max() / 10) return 0;
    divisor *= 10;
  }
  const SimDuration remainder = value % divisor;
  return (value / divisor) + (remainder >= divisor - remainder ? 1 : 0);
}

}  // namespace

auto MultiplyByPowerOfTen(SimDuration value, int exponent) noexcept
    -> SimDuration {
  SimDuration result = value;
  for (int i = 0; i < exponent; ++i) {
    if (result > std::numeric_limits<SimDuration>::max() / 10) {
      return std::numeric_limits<SimDuration>::max();
    }
    result *= 10;
  }
  return result;
}

auto StepsToTicks(
    SimDuration steps, std::int8_t unit_power,
    std::int8_t global_power) noexcept -> SimDuration {
  const int coarser_by = unit_power - global_power;
  return coarser_by >= 0 ? MultiplyByPowerOfTen(steps, coarser_by)
                         : DivideByPowerOfTen(steps, -coarser_by);
}

auto TicksToSteps(
    SimTime ticks, std::int8_t unit_power, std::int8_t global_power) noexcept
    -> SimTime {
  const int coarser_by = unit_power - global_power;
  return coarser_by >= 0 ? DivideByPowerOfTen(ticks, coarser_by)
                         : MultiplyByPowerOfTen(ticks, -coarser_by);
}

auto NowInUnit(RuntimeEffects& runtime, std::int8_t unit_power) -> SimTime {
  return TicksToSteps(
      runtime.Now(), unit_power, runtime.GlobalPrecisionPower());
}

auto SimTimeInUnit(RuntimeEffects& runtime, std::int64_t unit_power)
    -> value::Time {
  return value::Time::FromInt(
      static_cast<std::int64_t>(
          NowInUnit(runtime, static_cast<std::int8_t>(unit_power))));
}

auto STimeInUnit(RuntimeEffects& runtime, std::int64_t unit_power)
    -> value::IntUnsigned {
  const SimTime scaled =
      NowInUnit(runtime, static_cast<std::int8_t>(unit_power));
  return value::IntUnsigned::FromInt(
      static_cast<std::int64_t>(static_cast<std::uint32_t>(scaled)));
}

auto RealTimeInUnit(RuntimeEffects& runtime, std::int64_t unit_power)
    -> value::Real {
  const int coarser_by =
      static_cast<int>(unit_power) - runtime.GlobalPrecisionPower();
  const auto now = static_cast<double>(runtime.Now());
  return value::Real{
      coarser_by >= 0
          ? now / static_cast<double>(MultiplyByPowerOfTen(1, coarser_by))
          : now * static_cast<double>(MultiplyByPowerOfTen(1, -coarser_by))};
}

}  // namespace lyra::runtime
