#pragma once

#include <compare>
#include <cstdint>

namespace lyra::mir {

struct ExternalUnitObjectId {
  std::uint32_t value;

  auto operator<=>(const ExternalUnitObjectId&) const
      -> std::strong_ordering = default;
};

}  // namespace lyra::mir
