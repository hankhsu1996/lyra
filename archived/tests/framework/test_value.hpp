#pragma once

#include <cstdint>
#include <optional>
#include <string_view>
#include <variant>
#include <vector>

namespace lyra::test {

// Integral value with 4-state support.
// Encoding per bit: unknown=0,value=0->0; unknown=0,value=1->1;
//                   unknown=1,value=0->X; unknown=1,value=1->Z
// Word ordering: word[0] = least significant 64 bits (LSB-first).
// Within a word: bit 0 = LSB.
struct IntegralValue {
  uint32_t width;
  std::vector<uint64_t> value;    // Value/Z bits
  std::vector<uint64_t> unknown;  // Unknown bits (0 = known)
};

// Parse SV literal format: N'b..., N'h..., N'o... into IntegralValue.
// Returns std::nullopt if not in SV literal format or invalid.
// Supports underscores, X/Z digits, and all standard bases.
auto ParseSvLiteral(std::string_view str) -> std::optional<IntegralValue>;

// Canonical test value - what backends extract after simulation.
// All integrals are represented as IntegralValue (with proper width).
using TestValue = std::variant<double, IntegralValue>;

// Expected value - what YAML tests specify.
//
// uint64_t: Untyped integer (width inferred from actual during comparison).
//   Interpreted as a 64-bit bit pattern, masked to actual.width before
//   comparison. This is NOT mathematical integer equality - no sign extension.
//   Supported formats:
//     - YAML numbers: 42, -2 (negatives stored as 2's complement)
//     - C-style strings: "0xff", "0b1010", "0o377", "0xff_ff"
//   Note: Combining negatives with C-style bases (e.g., "-0xff") is not
//   supported.
//
// double: real/shortreal
//
// IntegralValue: SV literal (e.g., "8'hFF", "4'bx01z") - requires exact width
//   match with actual.
using ExpectedValue = std::variant<uint64_t, double, IntegralValue>;

}  // namespace lyra::test
