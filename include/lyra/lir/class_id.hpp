#pragma once

#include <compare>
#include <cstdint>

namespace lyra::lir {

struct ClassId {
  std::uint32_t value;

  auto operator<=>(const ClassId&) const -> std::strong_ordering = default;
};

}  // namespace lyra::lir
