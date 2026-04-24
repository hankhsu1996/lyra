#pragma once

#include <algorithm>
#include <cstdint>
#include <utility>

namespace lyra {

struct ConstantRange {
  int32_t left = 0;   // Source order (left bound from [left:right])
  int32_t right = 0;  // Source order (right bound from [left:right])

  [[nodiscard]] auto Lower() const -> int32_t {
    return std::min(left, right);
  }
  [[nodiscard]] auto Upper() const -> int32_t {
    return std::max(left, right);
  }
  [[nodiscard]] auto IsDescending() const -> bool {
    return left >= right;
  }
  [[nodiscard]] auto Size() const -> uint32_t {
    return static_cast<uint32_t>(Upper() - Lower() + 1);
  }

  auto operator==(const ConstantRange&) const -> bool = default;

  template <typename H>
  friend auto AbslHashValue(H h, const ConstantRange& r) -> H {
    return H::combine(std::move(h), r.left, r.right);
  }
};

}  // namespace lyra
