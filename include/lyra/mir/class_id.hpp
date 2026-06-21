#pragma once

#include <compare>
#include <cstdint>

namespace lyra::mir {

struct ClassId {
  std::uint32_t value;

  auto operator<=>(const ClassId&) const -> std::strong_ordering = default;
};

}  // namespace lyra::mir
