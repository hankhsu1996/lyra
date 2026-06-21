#pragma once

#include <compare>
#include <cstdint>

namespace lyra::mir {

struct EnclosingHops {
  std::uint32_t value = 0;

  auto operator<=>(const EnclosingHops&) const
      -> std::strong_ordering = default;
};

}  // namespace lyra::mir
