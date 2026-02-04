#pragma once

#include <format>
#include <string>
#include <string_view>

namespace lyra {

constexpr int kDefaultTimeScalePower = -12;  // 1ps (IEEE 1800 default)

/// Convert power of 10 to human-readable time unit string.
/// Examples: -9 -> "1ns", -8 -> "10ns", -10 -> "100ps"
inline auto PowerToString(int power) -> std::string {
  int base_power = 0;
  int magnitude = 0;
  if (power >= 0) {
    base_power = (power / 3) * 3;
    magnitude = power - base_power;
  } else {
    int quotient = (power - 2) / 3;
    base_power = quotient * 3;
    magnitude = power - base_power;
  }

  std::string_view prefix;
  switch (magnitude) {
    case 0:
      prefix = "1";
      break;
    case 1:
      prefix = "10";
      break;
    case 2:
      prefix = "100";
      break;
    default:
      prefix = "1";
      break;
  }

  std::string_view unit;
  switch (base_power) {
    case 0:
      unit = "s";
      break;
    case -3:
      unit = "ms";
      break;
    case -6:
      unit = "us";
      break;
    case -9:
      unit = "ns";
      break;
    case -12:
      unit = "ps";
      break;
    case -15:
      unit = "fs";
      break;
    default:
      return std::format("1e{}s", base_power);
  }

  return std::format("{}{}", prefix, unit);
}

/// Format a timescale message for $printtimescale output.
/// Returns: "Time scale of '<scope_name>' is <unit> / <prec>" (no newline)
inline auto FormatTimescale(
    std::string_view scope_name, int unit_power, int prec_power)
    -> std::string {
  return std::format(
      "Time scale of '{}' is {} / {}", scope_name, PowerToString(unit_power),
      PowerToString(prec_power));
}

}  // namespace lyra
