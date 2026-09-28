#pragma once

#include <bit>
#include <cstdint>

namespace lyra::hir {

// A real value (LRM 6.12, an IEEE 754 double) held as the bits that represent
// it. What a value is decides whether two pieces of HIR are one, and numeric
// comparison answers a different question: it calls 0.0 and -0.0 equal though
// dividing by each gives infinities of opposite sign, and calls a NaN unequal
// even to itself. Holding the bits makes the comparison the one of identity.
struct RealBits {
  std::uint64_t bits;

  [[nodiscard]] static auto Of(double value) -> RealBits {
    return RealBits{.bits = std::bit_cast<std::uint64_t>(value)};
  }

  [[nodiscard]] auto Value() const -> double {
    return std::bit_cast<double>(bits);
  }

  auto operator==(const RealBits&) const -> bool = default;
};

}  // namespace lyra::hir
