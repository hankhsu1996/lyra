#pragma once

#include <algorithm>
#include <cstdint>
#include <vector>

namespace lyra {

struct IntegralConstant {
  std::vector<uint64_t> value;    // Value/Z bits
  std::vector<uint64_t> unknown;  // Unknown bits (0 = known)

  auto operator==(const IntegralConstant&) const -> bool = default;

  template <typename H>
  friend auto AbslHashValue(H h, const IntegralConstant& c) -> H {
    return H::combine(std::move(h), c.value, c.unknown);
  }

  [[nodiscard]] auto IsKnown() const -> bool {
    return std::ranges::all_of(unknown, [](uint64_t w) { return w == 0; });
  }
};

}  // namespace lyra
