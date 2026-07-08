#pragma once

#include <compare>
#include <cstdint>
#include <variant>

#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/structural_data_object.hpp"
#include "lyra/hir/structural_hops.hpp"
#include "lyra/hir/with_clause_id.hpp"

namespace lyra::hir {

struct StructuralDataObjectRef {
  StructuralHops hops;
  StructuralDataObjectId var;

  auto operator==(const StructuralDataObjectRef&) const -> bool = default;
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

// A reference to a class property (LRM 8.4) from within an instance method
// body, where the property is named without an explicit handle and reaches the
// invoking object through the method's receiver. `field_index` is the
// property's declaration-order position in the enclosing class.
struct ClassPropertyRef {
  std::uint32_t field_index;

  auto operator==(const ClassPropertyRef&) const -> bool = default;
};

// A reference to a `with`-clause iteration value (LRM 7.12.4), named by the
// owning clause's identity and the role. Both element and index are closure
// parameters; HIR-to-MIR resolves this to that clause's parameter, capturing it
// when the reference sits inside a deeper clause's closure body.
struct IterationBindingRef {
  WithClauseId clause;
  IterationBindingRole role;

  auto operator==(const IterationBindingRef&) const -> bool = default;
};

// A reader-relative route to a referenced value: either a this-unit structural
// data object reached by a typed enclosing climb, or a cross-unit member
// reached through a resolved slot. One route serves every consumer of the
// reference -- value read, value write, and change observation -- so the name
// is neutral to the consumer, not owned by sensitivity.
using ReferenceRoute = std::variant<StructuralDataObjectRef, CrossUnitVarRef>;

}  // namespace lyra::hir
