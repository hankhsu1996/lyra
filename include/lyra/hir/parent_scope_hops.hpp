#pragma once

#include <compare>
#include <cstdint>

namespace lyra::hir {

struct ParentScopeHops {
  std::uint32_t value;

  auto operator<=>(const ParentScopeHops&) const
      -> std::strong_ordering = default;
};

}  // namespace lyra::hir
