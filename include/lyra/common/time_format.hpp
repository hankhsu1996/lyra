#pragma once

#include <cmath>
#include <cstdint>
#include <format>
#include <string>
#include <utility>

namespace lyra::common {

// State set by $timeformat system task (IEEE 1800-2017 §21.3).
// Controls how %t format specifier displays simulation time values.
struct TimeFormatState {
  int8_t units = kUnitsUnset;  // Power of 10 (-9 for ns, -12 for ps)
  int precision = 0;           // Decimal digits (0-17)
  std::string suffix;          // Appended after time value
  int min_width = 20;          // Minimum field width

  // Sentinel: units not set, will use input unit at format time.
  static constexpr int8_t kUnitsUnset = 127;

  [[nodiscard]] auto Format(uint64_t time_value, int8_t input_unit_power) const
      -> std::string {
    return FormatWithWidth(time_value, input_unit_power, min_width);
  }

  [[nodiscard]] auto FormatWithWidth(
      uint64_t time_value, int8_t input_unit_power, int field_width) const
      -> std::string {
    int8_t effective_units = (units == kUnitsUnset) ? input_unit_power : units;

    // Scale: 10^(input_unit_power - effective_units)
    // e.g., input=-9 (ns), effective=-12 (ps) -> multiply by 10^3
    int exponent = input_unit_power - effective_units;
    auto scaled = static_cast<double>(time_value);

    if (exponent > 0) {
      scaled *= std::pow(10.0, exponent);
    } else if (exponent < 0) {
      scaled /= std::pow(10.0, -exponent);
    }

    auto formatted = std::format("{:.{}f}{}", scaled, precision, suffix);

    if (std::cmp_less(formatted.size(), field_width)) {
      formatted =
          std::string(field_width - static_cast<int>(formatted.size()), ' ') +
          formatted;
    }

    return formatted;
  }
};

}  // namespace lyra::common
