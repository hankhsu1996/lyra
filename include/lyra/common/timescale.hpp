#pragma once

#include <algorithm>
#include <cmath>
#include <cstdint>
#include <optional>
#include <span>
#include <string>

#include <fmt/core.h>
#include <slang/numeric/Time.h>

#include "lyra/sdk/time_utils.hpp"

namespace lyra::common {

/// Represents a timescale with unit and precision as power-of-10 exponents.
///
/// The power values follow LRM Table 20-2:
///   2 = 100s, 1 = 10s, 0 = 1s, -1 = 100ms, ..., -9 = 1ns, ..., -15 = 1fs
///
/// Example: For `timescale 1ns / 1ps:
///   unit_power = -9 (1ns)
///   precision_power = -12 (1ps)
///
/// Overflow assumptions:
///   - Maximum practical exponent difference is ~15 (1s to 1fs), giving 10^15
///   - This fits in uint64_t (max ~1.8e19)
///   - Extreme timescales like 100s/1fs (exponent=17) may overflow in
///     delay calculations; such configurations are rare in practice
struct TimeScale {
  int8_t unit_power;       // Time unit as power of 10 (e.g., -9 for 1ns)
  int8_t precision_power;  // Time precision as power of 10 (must be <= unit)

  /// Default timescale: 1ns / 1ps (matches Verilator convention)
  static constexpr int8_t kDefaultUnitPower = -9;        // 1ns
  static constexpr int8_t kDefaultPrecisionPower = -12;  // 1ps

  /// Create a TimeScale from slang's TimeScale.
  static auto FromSlang(const slang::TimeScale& ts) -> TimeScale {
    return TimeScale{
        .unit_power = ToPowerOfTen(ts.base),
        .precision_power = ToPowerOfTen(ts.precision)};
  }

  /// Create default timescale (1ns / 1ps).
  static auto Default() -> TimeScale {
    return TimeScale{
        .unit_power = kDefaultUnitPower,
        .precision_power = kDefaultPrecisionPower};
  }

  /// Compute the multiplier to scale a delay from this timescale's unit
  /// to the global precision.
  ///
  /// Example: If unit is 1ns (-9) and global precision is 1ps (-12),
  /// the multiplier is 10^(-9 - (-12)) = 10^3 = 1000.
  [[nodiscard]] auto DelayMultiplier(int8_t global_precision_power) const
      -> uint64_t {
    int exponent = unit_power - global_precision_power;
    // Exponent should always be >= 0 since global_precision <= unit
    if (exponent < 0) {
      return 1;  // Shouldn't happen with valid timescales
    }
    return static_cast<uint64_t>(std::pow(10.0, exponent));
  }

  /// Compute the divisor to convert internal time (in global precision units)
  /// back to this module's time unit for $time/$realtime.
  ///
  /// Example: If unit is 1ns (-9) and global precision is 1ps (-12),
  /// the divisor is 1000 (divide internal ps by 1000 to get ns).
  ///
  /// Note: Mathematically identical to DelayMultiplier() - the same factor
  /// that scales delays up (timeunit -> precision) also scales time queries
  /// down (precision -> timeunit). Separate methods for semantic clarity at
  /// call sites.
  [[nodiscard]] auto TimeDivisor(int8_t global_precision_power) const
      -> uint64_t {
    return DelayMultiplier(global_precision_power);
  }

  [[nodiscard]] auto ToString() const -> std::string {
    return fmt::format(
        "{} / {}", sdk::PowerToString(unit_power),
        sdk::PowerToString(precision_power));
  }

  auto operator==(const TimeScale& other) const -> bool = default;

 private:
  /// Convert slang TimeScaleValue to power of 10.
  static auto ToPowerOfTen(const slang::TimeScaleValue& tsv) -> int8_t {
    // Base power from unit
    int8_t base = 0;
    switch (tsv.unit) {
      case slang::TimeUnit::Seconds:
        base = 0;
        break;
      case slang::TimeUnit::Milliseconds:
        base = -3;
        break;
      case slang::TimeUnit::Microseconds:
        base = -6;
        break;
      case slang::TimeUnit::Nanoseconds:
        base = -9;
        break;
      case slang::TimeUnit::Picoseconds:
        base = -12;
        break;
      case slang::TimeUnit::Femtoseconds:
        base = -15;
        break;
    }

    // Adjust for magnitude (1, 10, or 100)
    switch (tsv.magnitude) {
      case slang::TimeScaleMagnitude::One:
        break;
      case slang::TimeScaleMagnitude::Ten:
        base += 1;
        break;
      case slang::TimeScaleMagnitude::Hundred:
        base += 2;
        break;
    }

    return base;
  }
};

/// Compute the finest precision across multiple timescales.
///
/// Returns the smallest (most precise) precision power among all provided
/// timescales. If no timescale has a value, returns the default precision.
///
/// @param timescales A span of optional timescales from multiple modules
/// @return The global precision power (smallest value = finest precision)
inline auto ComputeGlobalPrecision(
    std::span<const std::optional<TimeScale>> timescales) -> int8_t {
  int8_t global = TimeScale::kDefaultPrecisionPower;
  for (const auto& ts : timescales) {
    if (ts) {
      global = std::min(global, ts->precision_power);
    }
  }
  return global;
}

}  // namespace lyra::common

template <>
struct fmt::formatter<lyra::common::TimeScale> {
  template <typename ParseContext>
  constexpr auto parse(ParseContext& ctx) {
    return ctx.begin();
  }

  template <typename FormatContext>
  auto format(const lyra::common::TimeScale& ts, FormatContext& ctx) const {
    return fmt::format_to(ctx.out(), "{}", ts.ToString());
  }
};
