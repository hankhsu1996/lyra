#pragma once

#include <compare>
#include <cstdint>

namespace lyra::mir {

struct BodyHops {
  std::uint32_t value = 0;

  auto operator<=>(const BodyHops&) const -> std::strong_ordering = default;
};

}  // namespace lyra::mir
