#pragma once

#include <compare>
#include <cstdint>
#include <variant>

#include "lyra/mir/procedural_hops.hpp"
#include "lyra/mir/procedural_var.hpp"
#include "lyra/mir/structural_hops.hpp"
#include "lyra/mir/structural_var.hpp"

namespace lyra::mir {

struct StructuralVarRef {
  StructuralHops hops;
  StructuralVarId var{};
};

struct ProceduralVarRef {
  ProceduralHops hops;
  ProceduralVarId var{};
};

struct CrossUnitRefId {
  std::uint32_t value;

  auto operator<=>(const CrossUnitRefId&) const
      -> std::strong_ordering = default;
};

// A reference into a child instance's member, resolved once at construction
// into a stored direct reference; the navigation recipe lives in the enclosing
// scope's `cross_unit_refs` table keyed by this id.
struct CrossUnitVarRef {
  CrossUnitRefId id;
};

// A sensitivity leaf observes either a this-unit structural var or a cross-unit
// member; both resolve to a stored `Observable` the scheduler subscribes to.
using SensitivityRef = std::variant<StructuralVarRef, CrossUnitVarRef>;

}  // namespace lyra::mir
