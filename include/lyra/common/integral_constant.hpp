#pragma once

#include <algorithm>
#include <cstdint>
#include <vector>

namespace lyra {

struct IntegralConstant {
  std::vector<uint64_t> a;  // Value/Z bits
  std::vector<uint64_t> b;  // Unknown bits (0 = known)

  auto operator==(const IntegralConstant&) const -> bool = default;

  template <typename H>
  friend auto AbslHashValue(H h, const IntegralConstant& c) -> H {
    return H::combine(std::move(h), c.a, c.b);
  }

  [[nodiscard]] auto IsKnown() const -> bool {
    return std::ranges::all_of(b, [](uint64_t w) { return w == 0; });
  }
};

}  // namespace lyra
