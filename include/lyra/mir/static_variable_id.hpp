#pragma once

#include <compare>
#include <cstdint>

namespace lyra::mir {

// Identity of a unit-level static variable -- a named, mutable value the unit's
// namespace owns with static storage, one program-global cell shared across the
// whole simulation (LRM 6.21 static lifetime, LRM 26.2 package variables). The
// data dual of a receiver-less namespace callable, scoped to the unit that
// declares it. The id keys the declaring unit's emission arena, while every
// reference reaches the cell by name (`unit::name`), so it is never carried in
// a reference expression.
struct StaticVariableId {
  std::uint32_t value;

  auto operator<=>(const StaticVariableId&) const
      -> std::strong_ordering = default;
};

}  // namespace lyra::mir
