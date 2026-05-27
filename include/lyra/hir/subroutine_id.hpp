#pragma once

#include <compare>
#include <cstdint>

namespace lyra::hir {

struct StructuralSubroutineId {
  std::uint32_t value;

  auto operator<=>(const StructuralSubroutineId&) const
      -> std::strong_ordering = default;
};

}  // namespace lyra::hir
