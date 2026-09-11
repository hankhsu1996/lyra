#pragma once

#include <compare>
#include <cstdint>

#include "lyra/base/pool_id.hpp"

namespace lyra::mir {

// Identity of a unit-level static variable -- a mutable value the unit's
// namespace owns with static storage, one program-global cell shared across the
// whole simulation (LRM 6.21 static lifetime, LRM 26.2 package variables). The
// data dual of a receiver-less namespace callable, scoped to the unit that
// declares it. A body of the declaring unit reaches the cell by this id, having
// the arena in hand; a body of any other unit has only the name that unit
// published for it.
struct StaticVariableId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const StaticVariableId&) const
      -> std::strong_ordering = default;
};

}  // namespace lyra::mir
