#pragma once

#include <compare>
#include <cstdint>

namespace lyra::lir {

struct ClosureId {
  std::uint32_t value;

  auto operator<=>(const ClosureId&) const -> std::strong_ordering = default;
};

}  // namespace lyra::lir
