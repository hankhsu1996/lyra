#pragma once

#include <compare>
#include <cstdint>
#include <string>

#include "lyra/mir/structural_hops.hpp"

namespace lyra::mir {

struct StructuralSubroutineId {
  std::uint32_t value;

  auto operator<=>(const StructuralSubroutineId&) const
      -> std::strong_ordering = default;
};

struct StructuralSubroutineDecl {
  std::string name;
};

struct StructuralSubroutineRef {
  StructuralHops hops = {};
  StructuralSubroutineId subroutine = {};
};

}  // namespace lyra::mir
