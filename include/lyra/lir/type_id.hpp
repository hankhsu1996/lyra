#pragma once

#include <compare>
#include <cstdint>

namespace lyra::lir {

struct TypeId {
  std::uint32_t value;

  auto operator<=>(const TypeId&) const -> std::strong_ordering = default;
};

}  // namespace lyra::lir
