#pragma once

#include <cstdint>
#include <variant>
#include <vector>

namespace lyra::test {

// Integral value with 4-state support.
// Encoding per bit: unknown=0,value=0→0; unknown=0,value=1→1;
//                   unknown=1,value=0→X; unknown=1,value=1→Z
// Word ordering: word[0] = least significant 64 bits (LSB-first).
// Within a word: bit 0 = LSB.
struct IntegralValue {
  uint32_t width;
  std::vector<uint64_t> value;    // Value/Z bits
  std::vector<uint64_t> unknown;  // Unknown bits (0 = known)
};

// Canonical test value - what backends extract after simulation.
// All integrals are represented as IntegralValue (with proper width).
using TestValue = std::variant<double, IntegralValue>;

// Expected value - what YAML tests specify.
// Extends TestValue with int64_t for convenience (e.g., "result: 42").
// int64_t: narrow integer literals (width inferred from actual during
// comparison) double: real/shortreal IntegralValue: SV literal format (e.g.,
// "8'hFF", "4'bx01z")
using ExpectedValue = std::variant<int64_t, double, IntegralValue>;

}  // namespace lyra::test
