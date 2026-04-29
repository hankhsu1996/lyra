#pragma once

#include <compare>
#include <cstdint>

namespace lyra::hir {

struct ExprId {
  std::uint32_t value;

  auto operator<=>(const ExprId&) const -> std::strong_ordering = default;
};

}  // namespace lyra::hir
