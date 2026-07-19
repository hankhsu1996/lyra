#pragma once

#include <compare>
#include <cstdint>
#include <string>
#include <variant>

#include "lyra/hir/class_id.hpp"
#include "lyra/hir/field_id.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/static_property_id.hpp"
#include "lyra/hir/structural_data_object.hpp"
#include "lyra/hir/type_id.hpp"
#include "lyra/hir/with_clause_id.hpp"

namespace lyra::hir {

// A reference to a structural data object (variable or net) that sits directly
// on the reader's own scope: an empty route, reached as a plain member of
// `self`. Any target that needs a route to reach -- an enclosing ancestor
// member, a sibling or child scope, another compilation unit -- is a RoutedRef
// instead.
struct DirectMemberRef {
  StructuralDataObjectId var;

  auto operator==(const DirectMemberRef&) const -> bool = default;
};

struct RoutedRefId {
  std::uint32_t value;

  auto operator<=>(const RoutedRefId&) const -> std::strong_ordering = default;
};

// A reference reached through a non-empty route: an enclosing ancestor member,
// a sibling or child scope, or another compilation unit. The route resolves
// once in the resolve phase into a stored per-instance endpoint -- a borrowed
// pointer to the target's cell -- and every read, write, and observation
// dereferences that one sealed endpoint. The navigation recipe lives in the
// owning scope's routed-reference table keyed by this id. Intra-unit and
// cross-unit references differ only in each segment's classification during
// resolve (typed member access vs by-name runtime lookup), not in the
// endpoint representation.
struct RoutedRef {
  RoutedRefId id;

  auto operator==(const RoutedRef&) const -> bool = default;
};

struct ProceduralVarRef {
  ProceduralVarId var;

  auto operator==(const ProceduralVarRef&) const -> bool = default;
};

// A reference to a class property (LRM 8.4) from within an instance method
// body, where the property is named without an explicit handle and reaches
// the invoking object through the method's receiver. `owner` is the class
// that declares the property, and `field_index` is its declaration-order
// position within that class's property arena. Under inheritance (LRM 8.13)
// a bare name may resolve to a property declared on an ancestor class, so
// identity is owner-qualified rather than derived from the enclosing method
// body's class.
struct ClassPropertyRef {
  ClassId owner;
  FieldId field_index;

  auto operator==(const ClassPropertyRef&) const -> bool = default;
};

// A reference to a class static property (LRM 8.9), the type-associated
// storage counterpart to `ClassPropertyRef`. `owner` names the class that
// declares the property; `prop` is its position within that class's
// static-property arena. A static property is one cell owned by the type, not
// a member replicated into each instance, so this reference carries no
// receiver: the source form `Cls::prop`, an unqualified use inside a method
// of the same class, and `p.prop` where the resolved target happens to be
// static all resolve to the same cell and the same reference shape.
// Under inheritance, `Derived::inherited_prop` still names the base class as
// `owner` -- the property lives on the base's arena.
struct StaticPropertyRef {
  ClassId owner;
  StaticPropertyId prop;

  auto operator==(const StaticPropertyRef&) const -> bool = default;
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

// A reference to a variable that belongs to another compilation unit's
// namespace -- a package variable (LRM 26.2), reached by name. Like
// `ExternalUnitSubroutineRef` for a call, it carries no unit-local id: a
// package has no instance and no receiver, so its variable is named and
// resolved against the target unit's interface at link time, never through a
// `self`-based route within any unit. The same by-name form serves a referrer
// in another unit and the package's own callable reading its own variable,
// since neither has a receiver to reach it through. One form serves read,
// write, and observation.
struct ExternalUnitValueRef {
  std::string unit_name;
  std::string variable_name;
  // The variable's value type. Carried on the node so it is self-describing: a
  // sensitivity leaf reaches the observed cell's type from the reference alone,
  // with no enclosing expression to type it.
  TypeId value_type;

  auto operator==(const ExternalUnitValueRef&) const -> bool = default;
};

// A reader-relative reference to a value: either a direct member of the
// reader's own scope, or a routed reference sealed to a per-instance endpoint
// in the resolve phase. One route serves every consumer of the reference --
// value read, value write, and change observation -- so the name is neutral to
// the consumer, not owned by sensitivity.
using ReferenceRoute = std::variant<DirectMemberRef, RoutedRef>;

// What a sensitivity leaf watches: a reader-relative route to an intra-unit
// observable cell, or a by-name reference to a package variable's one
// program-global cell (LRM 26.2). A package variable's change wakes the process
// the same way an intra-unit signal's does, but it is reached by name, not
// through a route, since a package has no per-instance storage to route to.
using SensitivityTarget = std::variant<ReferenceRoute, ExternalUnitValueRef>;

}  // namespace lyra::hir
