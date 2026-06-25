#pragma once

#include <compare>
#include <cstdint>

namespace lyra::mir {

struct MethodId {
  std::uint32_t value;

  auto operator<=>(const MethodId&) const -> std::strong_ordering = default;
};

}  // namespace lyra::mir
