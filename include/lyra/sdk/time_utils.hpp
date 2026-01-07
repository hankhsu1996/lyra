#pragma once

#include <cstdint>
#include <format>
#include <string>

namespace lyra::sdk {

/// Convert power of 10 to human-readable time unit string.
///
/// Examples:
///   -9 → "1ns", -8 → "10ns", -10 → "100ps"
///   0 → "1s", 1 → "10s", 2 → "100s"
///
/// Used by TimeScale::ToString() and $printtimescale.
inline auto PowerToString(int8_t power) -> std::string {
  // Find the base unit (s, ms, us, ns, ps, fs) and magnitude (1, 10, 100)
  // base = floor(power / 3) * 3, magnitude = power - base
  // For power = -8: base = -9 (ns), magnitude = 1 (10)
  // For power = -10: base = -12 (ps), magnitude = 2 (100)
  int base_power = 0;
  int magnitude = 0;
  if (power >= 0) {
    base_power = (power / 3) * 3;
    magnitude = power - base_power;
  } else {
    // Floor division for negatives: C++ integer division truncates toward zero,
    // but we need floor (toward -infinity). Formula: floor(x/n) = (x - (n-1)) /
    // n Example: power=-8 → quotient=(-8-2)/3=-10/3=-3 → base=-9, magnitude=1
    int quotient = (power - 2) / 3;
    base_power = quotient * 3;
    magnitude = power - base_power;
  }

  std::string prefix;
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
      prefix = "1";  // Fallback
      break;
  }

  std::string unit;
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
      unit = std::format("e{}s", base_power);
      break;
  }

  return prefix + unit;
}

}  // namespace lyra::sdk
