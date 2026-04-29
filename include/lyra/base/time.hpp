#pragma once

#include <cstdint>

namespace lyra {

using SimTime = std::uint64_t;
using SimDuration = std::uint64_t;

inline constexpr std::int8_t kDefaultTimeUnitPower = -9;
inline constexpr std::int8_t kDefaultTimePrecisionPower = -9;

struct TimeResolution {
  std::int8_t unit_power = kDefaultTimeUnitPower;
  std::int8_t precision_power = kDefaultTimePrecisionPower;
};

}  // namespace lyra
