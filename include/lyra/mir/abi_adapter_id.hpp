#pragma once

#include <compare>
#include <cstdint>

namespace lyra::mir {

struct AbiAdapterId {
  std::uint32_t value;

  auto operator<=>(const AbiAdapterId&) const -> std::strong_ordering = default;
};

}  // namespace lyra::mir
