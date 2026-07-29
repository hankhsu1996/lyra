#pragma once

#include <compare>
#include <cstdint>
#include <string>
#include <variant>

#include "lyra/hir/class_ref.hpp"
#include "lyra/hir/pattern_id.hpp"
#include "lyra/hir/procedural_var.hpp"
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
// the invoking object through the method's receiver. `target` names the
// declaring class and the slot within its property arena. Owner-qualified
// (not derived from the enclosing method body's class) because under
// inheritance (LRM 8.13) a bare name may resolve to a property declared on
// an ancestor class. The external arm is used when the property's declaring
// class lives in another compilation unit.
struct ClassPropertyRef {
  ClassPropertyTarget target;

  auto operator==(const ClassPropertyRef&) const -> bool = default;
};

// A reference to a class static property (LRM 8.9). `target` names the
// declaring class and the slot within its static-property arena. A static
// property is one cell owned by the type, not a member replicated into each
// instance, so this reference carries no receiver: the source form
// `Cls::prop`, an unqualified use inside a method of the same class, and
// `p.prop` where the resolved target happens to be static all resolve to the
// same cell and the same reference shape. Under inheritance,
// `Derived::inherited_prop` still names the base class -- the property lives
// on the base's arena. The external arm is used when the declaring class
// lives in another compilation unit.
struct StaticPropertyRef {
  StaticPropertyTarget target;

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

// A reference to the identifier a pattern binds (LRM 12.6). The pattern
// introduces the scope the identifier is declared in, so the declaration is
// the `VariablePattern` node itself and its `PatternId` is the identity a
// reference carries -- there is no separate variable arena to index. That
// holds wherever the pattern appears: a case item, an if predicate, or a
// conditional expression, in a procedural body or a structural one.
struct PatternVarRef {
  PatternId pattern = {};

  auto operator==(const PatternVarRef&) const -> bool = default;
};

// A reference to a variable declared in a namespace unit -- a package (LRM
// 26.2) or the anonymous `$unit` scope (LRM 3.12.1) -- reached by name. Such a
// unit has no instance and no receiver, so its variable is one program-global
// cell, resolved against that unit's interface at link time rather than reached
// by a route out of anyone's storage. The same by-name form serves a referrer
// in another unit and the declaring unit's own body, neither of which has a
// receiver to reach it through.
struct ExternalUnitValueRef {
  std::string unit_name;
  std::string variable_name;
  // The cell's type. The declaring unit compiles separately, so no member of
  // this unit states it; it crosses as part of what this unit knows of that
  // unit's interface.
  TypeId value_type;

  auto operator==(const ExternalUnitValueRef&) const -> bool = default;
};

// A reader-relative reference to a value: either a direct member of the
// reader's own scope, or a routed reference sealed to a per-instance endpoint
// in the resolve phase.
using ReferenceRoute = std::variant<DirectMemberRef, RoutedRef>;

// Where a value's cell is, as the reader reaches it: through a reader-relative
// route to a cell in the reader's own unit, or by name across the boundary to a
// namespace unit's one program-global cell (LRM 26.2, 3.12.1), which has no
// per-instance storage to route to. One target serves every consumer of the
// reference -- value read, value write, and change observation -- so the name
// is neutral to the consumer and owned by none of them. A value with no cell at
// all has no target: a compile-time constant folds where it is used, leaving
// nothing to read through and nothing to observe.
using ValueTarget = std::variant<ReferenceRoute, ExternalUnitValueRef>;

}  // namespace lyra::hir
