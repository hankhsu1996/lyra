#pragma once

#include <compare>
#include <cstdint>

#include "lyra/base/pool_id.hpp"

namespace lyra::mir {

// Identity of a class static property (LRM 8.9) -- a named, mutable
// type-associated storage cell the class owns, shared by every instance.
// Scoped to the class that declares it; the type-associated counterpart of an
// instance member's `FieldId`.
struct StaticPropertyId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const StaticPropertyId&) const
      -> std::strong_ordering = default;
};

}  // namespace lyra::mir
