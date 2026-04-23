#pragma once

#include <compare>
#include <cstdint>

namespace lyra::hir {

struct ParamDeclId {
  std::uint32_t value;

  auto operator<=>(const ParamDeclId&) const -> std::strong_ordering = default;
};

}  // namespace lyra::hir
