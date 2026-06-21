#pragma once

#include <compare>
#include <cstdint>

namespace lyra::mir {

struct BlockHops {
  std::uint32_t value = 0;

  auto operator<=>(const BlockHops&) const -> std::strong_ordering = default;
};

}  // namespace lyra::mir
