#pragma once

#include <compare>
#include <cstdint>

#include "lyra/mir/structural_hops.hpp"

namespace lyra::mir {

struct StructuralSubroutineId {
  std::uint32_t value;

  auto operator<=>(const StructuralSubroutineId&) const
      -> std::strong_ordering = default;
};

struct StructuralSubroutineRef {
  StructuralHops hops = {};
  StructuralSubroutineId subroutine = {};
};

}  // namespace lyra::mir
