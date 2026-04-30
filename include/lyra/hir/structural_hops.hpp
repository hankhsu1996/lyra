#pragma once

#include <compare>
#include <cstdint>

namespace lyra::hir {

struct StructuralHops {
  std::uint32_t value;

  auto operator<=>(const StructuralHops&) const
      -> std::strong_ordering = default;
};

}  // namespace lyra::hir
