#pragma once

#include <compare>
#include <cstdint>

namespace lyra::hir {

struct NetDeclId {
  std::uint32_t value;

  auto operator<=>(const NetDeclId&) const -> std::strong_ordering = default;
};

}  // namespace lyra::hir
