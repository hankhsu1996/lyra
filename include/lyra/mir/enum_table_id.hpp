#pragma once

#include <compare>
#include <cstdint>

#include "lyra/base/pool_id.hpp"

namespace lyra::mir {

// Identity of one enumeration's member table within a compilation unit. Derived
// from the members rather than conferred on them: two declarations of the same
// members reach one entry, so equal ids mean one table.
struct EnumTableId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const EnumTableId&) const -> std::strong_ordering = default;
};

}  // namespace lyra::mir
