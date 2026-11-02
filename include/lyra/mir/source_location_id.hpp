#pragma once

#include <compare>
#include <cstdint>

#include "lyra/base/pool_id.hpp"

namespace lyra::mir {

// Identity of one location a unit's program states as a constant, within the
// unit. Derived from the location rather than conferred on it: two calls
// starting at one location reach one entry, so equal ids mean one constant.
struct SourceLocationId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const SourceLocationId&) const
      -> std::strong_ordering = default;
};

}  // namespace lyra::mir
