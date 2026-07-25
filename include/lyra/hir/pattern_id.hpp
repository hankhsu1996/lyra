#pragma once

#include <compare>
#include <cstdint>

namespace lyra::hir {

struct PatternId {
  std::uint32_t value;

  auto operator<=>(const PatternId&) const -> std::strong_ordering = default;
};

}  // namespace lyra::hir
