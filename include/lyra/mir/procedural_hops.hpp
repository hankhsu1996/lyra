#pragma once

#include <compare>
#include <cstdint>

namespace lyra::mir {

struct ProceduralHops {
  std::uint32_t value = 0;

  auto operator<=>(const ProceduralHops&) const
      -> std::strong_ordering = default;
};

}  // namespace lyra::mir
