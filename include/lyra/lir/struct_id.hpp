#pragma once

#include <compare>
#include <cstdint>

namespace lyra::lir {

struct StructId {
  std::uint32_t value;

  auto operator<=>(const StructId&) const -> std::strong_ordering = default;
};

}  // namespace lyra::lir
