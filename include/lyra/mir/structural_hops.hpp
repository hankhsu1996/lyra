#pragma once

#include <compare>
#include <cstdint>

namespace lyra::mir {

struct StructuralHops {
  std::uint32_t value = 0;

  auto operator<=>(const StructuralHops&) const
      -> std::strong_ordering = default;
};

}  // namespace lyra::mir
