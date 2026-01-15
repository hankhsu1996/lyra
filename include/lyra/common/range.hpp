#pragma once

#include <cstdint>
#include <utility>

namespace lyra {

struct ConstantRange {
  int32_t lower = 0;
  int32_t upper = 0;

  [[nodiscard]] auto Size() const -> uint32_t {
    return static_cast<uint32_t>(upper - lower + 1);
  }

  auto operator==(const ConstantRange&) const -> bool = default;

  template <typename H>
  friend auto AbslHashValue(H h, const ConstantRange& r) -> H {
    return H::combine(std::move(h), r.lower, r.upper);
  }
};

}  // namespace lyra
