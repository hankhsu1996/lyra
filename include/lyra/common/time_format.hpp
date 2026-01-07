#pragma once

#include <cmath>
#include <cstdint>
#include <format>
#include <string>
#include <utility>

namespace lyra::common {

/// Default timescale constants for SDK use.
/// These match the values in timescale.hpp but are duplicated here to avoid
/// pulling in slang dependencies into SDK code.
struct TimeScaleDefaults {
  static constexpr int8_t kDefaultUnitPower = -9;        // 1ns
  static constexpr int8_t kDefaultPrecisionPower = -12;  // 1ps
};

/// Represents the state set by $timeformat system task.
///
/// Per IEEE 1800-2017 §21.3, $timeformat controls how %t format specifier
/// displays simulation time values. Arguments:
///   - units: time unit as power of 10 (-15 to 2)
///   - precision: number of decimal digits (0-17)
///   - suffix: string appended to time value
///   - min_width: minimum field width
///
/// Default values (per LRM): units = simulation precision, precision = 0,
/// suffix = "", min_width = 20
struct TimeFormatState {
  /// Time unit as power of 10 (e.g., -9 for ns, -12 for ps).
  /// Initialized to a sentinel value; actual default is global precision.
  int8_t units = kUnitsUnset;

  /// Number of decimal digits to display (0-17).
  int precision = 0;

  /// String appended after the time value.
  std::string suffix;

  /// Minimum total field width for output.
  int min_width = 20;

  /// Sentinel value indicating units has not been set.
  /// Will be replaced with global precision at format time.
  static constexpr int8_t kUnitsUnset = 127;

  /// Format a raw simulation time to a string.
  ///
  /// @param raw_time Simulation time in global precision units
  /// @param global_precision The global precision power (e.g., -12 for 1ps)
  /// @return Formatted time string with scaling, precision, suffix, and padding
  [[nodiscard]] auto Format(uint64_t raw_time, int8_t global_precision) const
      -> std::string {
    // Use global precision if units not explicitly set
    int8_t effective_units = (units == kUnitsUnset) ? global_precision : units;

    // Calculate scaling factor: 10^(global_precision - units)
    // If global_precision = -12 (ps) and units = -9 (ns), exponent = -3
    // So we divide by 10^3 = 1000 to convert ps to ns
    int exponent = global_precision - effective_units;
    auto scaled = static_cast<double>(raw_time);

    if (exponent != 0) {
      // Use pow for the scaling
      scaled /= std::pow(10.0, exponent);
    }

    // Format with specified precision
    auto formatted = std::format("{:.{}f}{}", scaled, precision, suffix);

    // Pad to minimum width (right-aligned)
    if (std::cmp_less(formatted.size(), min_width)) {
      formatted =
          std::string(min_width - static_cast<int>(formatted.size()), ' ') +
          formatted;
    }

    return formatted;
  }

  /// Format a time value that's already in module's timeunit.
  /// This is used for %t where the argument comes from $time (scaled value).
  ///
  /// @param time_in_module_unit Time value in module's timeunit (e.g., from
  /// $time)
  /// @param module_unit_power The module's timeunit power (e.g., -9 for 1ns)
  /// @param global_precision The global precision power (for default units)
  [[nodiscard]] auto FormatModuleTime(
      uint64_t time_in_module_unit, int8_t module_unit_power,
      int8_t global_precision) const -> std::string {
    // Use global precision if units not explicitly set
    int8_t effective_units = (units == kUnitsUnset) ? global_precision : units;

    // Convert from module's timeunit to effective_units
    // If module_unit = -9 (ns) and effective_units = -12 (ps), exponent = 3
    // So we multiply by 10^3 = 1000 to convert ns to ps
    int exponent = module_unit_power - effective_units;
    auto scaled = static_cast<double>(time_in_module_unit);

    if (exponent > 0) {
      scaled *= std::pow(10.0, exponent);
    } else if (exponent < 0) {
      scaled /= std::pow(10.0, -exponent);
    }

    // Format with specified precision
    auto formatted = std::format("{:.{}f}{}", scaled, precision, suffix);

    // Pad to minimum width (right-aligned)
    if (std::cmp_less(formatted.size(), min_width)) {
      formatted =
          std::string(min_width - static_cast<int>(formatted.size()), ' ') +
          formatted;
    }

    return formatted;
  }
};

}  // namespace lyra::common
