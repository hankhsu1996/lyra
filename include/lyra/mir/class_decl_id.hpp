#pragma once

#include <compare>
#include <cstdint>

namespace lyra::mir {

// Owner-local index into the current container's `classes_`.
struct ClassDeclId {
  std::uint32_t value;

  auto operator<=>(const ClassDeclId&) const -> std::strong_ordering = default;
};

}  // namespace lyra::mir
