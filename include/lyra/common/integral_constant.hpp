#pragma once

#include <cstdint>
#include <vector>

namespace lyra {

struct IntegralConstant {
  std::vector<uint64_t> value;   // Value bits
  std::vector<uint64_t> x_mask;  // X (unknown) mask
  std::vector<uint64_t> z_mask;  // Z (high-impedance) mask

  auto operator==(const IntegralConstant&) const -> bool = default;

  template <typename H>
  friend auto AbslHashValue(H h, const IntegralConstant& c) -> H {
    return H::combine(std::move(h), c.value, c.x_mask, c.z_mask);
  }

  [[nodiscard]] auto IsKnown() const -> bool {
    for (size_t i = 0; i < x_mask.size(); ++i) {
      if (x_mask[i] != 0 || z_mask[i] != 0) {
        return false;
      }
    }
    return true;
  }
};

}  // namespace lyra
