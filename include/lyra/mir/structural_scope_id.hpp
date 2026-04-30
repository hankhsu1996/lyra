#pragma once

#include <compare>
#include <cstdint>

namespace lyra::mir {

struct StructuralScopeId {
  std::uint32_t value;

  auto operator<=>(const StructuralScopeId&) const
      -> std::strong_ordering = default;
};

}  // namespace lyra::mir
