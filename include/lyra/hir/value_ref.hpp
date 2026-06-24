#pragma once

#include <compare>
#include <cstdint>
#include <variant>

#include "lyra/hir/loop_var.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/structural_hops.hpp"
#include "lyra/hir/structural_var.hpp"

namespace lyra::hir {

struct StructuralVarRef {
  StructuralHops hops;
  StructuralVarId var;

  auto operator==(const StructuralVarRef&) const -> bool = default;
};

struct CrossUnitRefId {
  std::uint32_t value;

  auto operator<=>(const CrossUnitRefId&) const
      -> std::strong_ordering = default;
};

// A reference whose target lives in another compilation unit (a child
// instance's member). Resolved once at construction into a stored direct
// reference; the navigation recipe lives in the enclosing scope's
// `cross_unit_refs` table keyed by this id.
struct CrossUnitVarRef {
  CrossUnitRefId id;

  auto operator==(const CrossUnitVarRef&) const -> bool = default;
};

struct ProceduralVarRef {
  ProceduralVarId var;

  auto operator==(const ProceduralVarRef&) const -> bool = default;
};

struct LoopVarRef {
  StructuralHops hops;
  LoopVarDeclId loop_var;

  auto operator==(const LoopVarRef&) const -> bool = default;
};

// A sensitivity leaf observes either a this-unit structural var or a cross-unit
// member; both resolve to a stored `Observable` the scheduler subscribes to.
using SensitivityRef = std::variant<StructuralVarRef, CrossUnitVarRef>;

}  // namespace lyra::hir
