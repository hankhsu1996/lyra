#pragma once

#include <compare>
#include <cstdint>
#include <string>
#include <variant>
#include <vector>

#include "lyra/base/pool_id.hpp"
#include "lyra/hir/class_ref.hpp"
#include "lyra/hir/pattern_id.hpp"
#include "lyra/hir/procedural_scope.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/published_scope.hpp"
#include "lyra/hir/scope_class_id.hpp"
#include "lyra/hir/structural_hops.hpp"
#include "lyra/hir/type_id.hpp"
#include "lyra/hir/with_clause_id.hpp"
#include "lyra/support/def_path.hpp"

namespace lyra::hir {

// A name that reaches something in the design hierarchy: a declaration of the
// reader's own scope, of one enclosing it (LRM 23.9), or of any other scope on
// the tree, in this unit or another (LRM 23.6, 23.8). Where the target stands
// is the route -- how many scopes out, then which scopes down -- and the
// reader's own scope is the route that goes nowhere. The route lives in the
// owning scope's table for the use the name is put to, keyed by the id here.
// Every segment is a typed member access: a scope of this unit by its own
// declaration, a scope of another unit by what that unit published of it.
//
// What a name is used for decides what it may reach, and the source's position
// decides the use (LRM 23.6, 9.6.2), so each use is a reference of its own:
// a value read, written or waited on, which ends at data; a name a view defines
// as storage, which ends at each thing it stands for; a name a view offers as a
// value, computed on the instance it ends at; the object a call is made on; and
// what a `disable` ends. Which of the two a view's name takes is what the view
// declared it to be (LRM 25.5.4), and not where the source wrote it.
struct RoutedValueRefId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const RoutedValueRefId&) const
      -> std::strong_ordering = default;
};

struct RoutedValueRef {
  RoutedValueRefId id;

  auto operator<=>(const RoutedValueRef&) const
      -> std::strong_ordering = default;
};

struct RoutedViewPlaceRefId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const RoutedViewPlaceRefId&) const
      -> std::strong_ordering = default;
};

struct RoutedViewPlaceRef {
  RoutedViewPlaceRefId id;

  auto operator<=>(const RoutedViewPlaceRef&) const
      -> std::strong_ordering = default;
};

struct RoutedViewValueRefId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const RoutedViewValueRefId&) const
      -> std::strong_ordering = default;
};

struct RoutedViewValueRef {
  RoutedViewValueRefId id;

  auto operator<=>(const RoutedViewValueRef&) const
      -> std::strong_ordering = default;
};

struct RoutedObjectRefId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const RoutedObjectRefId&) const
      -> std::strong_ordering = default;
};

struct RoutedObjectRef {
  RoutedObjectRefId id;

  auto operator<=>(const RoutedObjectRef&) const
      -> std::strong_ordering = default;
};

// How a body reaches the instance a class belongs to (LRM 6.22), where a
// structural scope declares the class: `hops` scopes out of the body's own,
// where this unit lays the declaring scope out, and otherwise the route to that
// scope's object in another instance.
using DeclaringInstanceReach = std::variant<StructuralHops, RoutedObjectRef>;

struct RoutedDisableTargetRefId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const RoutedDisableTargetRefId&) const
      -> std::strong_ordering = default;
};

struct RoutedDisableTargetRef {
  RoutedDisableTargetRefId id;

  auto operator<=>(const RoutedDisableTargetRef&) const
      -> std::strong_ordering = default;
};

// The scope a `disable` or an assertion control task names, when it belongs to
// the declaration scope the statement's own body does. The identity indexes
// that scope's registry, so naming it needs no navigation at all -- the zero
// case of a reach, and the only form available inside a class method or a
// package subroutine, neither of which stands on the design hierarchy a route
// walks.
struct DirectDisableTarget {
  ProceduralScopeId scope;

  auto operator==(const DirectDisableTarget&) const -> bool = default;
};

// The scope a `disable` or an assertion control task names elsewhere on the
// elaborated hierarchy (LRM 23.6): a route to it, walked the way any other
// name's route is. Which of the two forms a statement carries says where the
// target lives and nothing about how the source spelled it.
struct RoutedDisableTarget {
  RoutedDisableTargetRef target;

  auto operator==(const RoutedDisableTarget&) const -> bool = default;
};

using DisableTarget = std::variant<DirectDisableTarget, RoutedDisableTarget>;

// Every instance of an array of instances, each reached as a name selecting it
// would reach it. How many there are and where each stands is what the
// declaration fixed (LRM 23.3.2).
struct RoutedObjectsRef {
  std::vector<RoutedObjectRef> objects;

  auto operator==(const RoutedObjectsRef&) const -> bool = default;
};

// Every block of a loop generate construct the object `holder` reaches built,
// which is that object's construction (LRM 27.4), so the loop is named by where
// the scope holding it published it.
struct LoopBlocksRef {
  RoutedObjectRef holder;
  ScopeClassId scope_class;
  PublishedLoopId loop;

  auto operator==(const LoopBlocksRef&) const -> bool = default;
};

// What a scope named as an argument of a task that takes scopes or assertions
// rather than values stands for (LRM 20.11): one object of the hierarchy, the
// objects of an array, the blocks of a loop, or the extent of a named block or
// a subroutine -- a statement label's block among them (LRM 16.3) -- reached as
// a `disable` naming it would reach it. Only that task reads it.
using ScopeArgument = std::variant<
    RoutedObjectRef, RoutedObjectsRef, LoopBlocksRef, DisableTarget>;

struct ScopeArgumentRef {
  ScopeArgument scope;

  auto operator==(const ScopeArgumentRef&) const -> bool = default;
};

struct ProceduralVarRef {
  ProceduralVarId var;

  auto operator<=>(const ProceduralVarRef&) const
      -> std::strong_ordering = default;
};

// A reference to a class property (LRM 8.4) from within an instance method
// body, where the property is named without an explicit handle and reaches
// the invoking object through the method's receiver. `target` names the
// declaring class and the slot within its property arena. Owner-qualified
// (not derived from the enclosing method body's class) because under
// inheritance (LRM 8.13) a bare name may resolve to a property declared on
// an ancestor class.
struct ClassPropertyRef {
  ClassPropertyTarget target;

  auto operator==(const ClassPropertyRef&) const -> bool = default;
};

// A static property (LRM 8.9) of a class a structural scope of this unit
// declares: the property, and how far out of the body's own structural scope
// the instance keeping its cell sits.
struct InstanceStaticPropertyTarget {
  ClassStaticPropertyTarget property;
  StructuralHops hops;

  auto operator<=>(const InstanceStaticPropertyTarget&) const
      -> std::strong_ordering = default;
};

// A static property is a cell of an instance where a structural scope
// replicates its class, and otherwise one cell the class keeps, whichever unit
// declares the class.
using StaticPropertyTarget =
    std::variant<InstanceStaticPropertyTarget, ClassStaticPropertyTarget>;

// A reference to a class static property (LRM 8.9). `target` names the
// declaring class and the slot within its static-property arena. A static
// property is one cell owned by the type, not a member replicated into each
// object of it, so this reference carries no receiver: the source form
// `Cls::prop`, an unqualified use inside a method of the same class, and
// `p.prop` where the resolved target happens to be static all resolve to the
// same cell and the same reference shape. Under inheritance,
// `Derived::inherited_prop` still names the base class -- the property lives
// on the base's arena.
//
// How many such cells exist follows from how many times the class declaration
// is replicated, which is what the arms of `target` tell apart. A class a
// namespace unit declares has one cell, the program's, named by the class,
// whichever unit declares it. A class a structural scope of this unit declares
// is a type of each instance of that scope (LRM 6.22), so each instance has
// that type's cell of its own, reached through the instance. One another design
// element declares is a cell of that element's instance, which a route reaches
// rather than this reference. What the cell holds is what the class's
// declaration states of the property.
struct StaticPropertyRef {
  StaticPropertyTarget target;

  auto operator<=>(const StaticPropertyRef&) const
      -> std::strong_ordering = default;
};

// A reference to a `with`-clause iteration value (LRM 7.12.4), named by the
// owning clause's identity and the role. Both element and index are variables
// of the loop the method is lowered to, so a clause nested in another's
// expression reads the outer one's by that identity, as any variable is read.
struct IterationBindingRef {
  WithClauseId clause;
  IterationBindingRole role = IterationBindingRole::kElement;

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

// A reference to a variable a namespace unit -- a package (LRM 26.2) or the
// anonymous `$unit` scope (LRM 3.12.1) -- keeps, reached by name: one it
// declares, or a static variable one of its subroutines declares (LRM 6.21),
// named through the subroutine. Such a unit has no instance and no receiver,
// so its variable is one program-global cell, resolved against that unit's
// signature at link time rather than reached by a route out of anyone's
// storage. The same by-name form serves a referrer in another unit and the
// declaring unit's own bodies, none of which has a receiver to reach it
// through.
struct NamespaceValueRef {
  std::string unit_name;
  support::QualifiedName variable;
  // The cell's type. A referrer in another unit declares nothing of the cell,
  // so the type crosses as part of the declaring unit's signature.
  TypeId value_type;

  auto operator<=>(const NamespaceValueRef&) const
      -> std::strong_ordering = default;
};

// Where a value's cell is, as the reader reaches it: through a reader-relative
// route to a cell on the design hierarchy, by name across the boundary to a
// namespace unit's one program-global cell (LRM 26.2, 3.12.1), which has no
// per-instance storage to route to, or on whatever replicates a class
// declaration, for a cell the type owns rather than any object of it (LRM 8.9).
// One target serves every consumer of the reference -- value read, value write,
// and change observation -- so the name is neutral to the consumer and owned by
// none of them. A value with no cell at all has no target: a compile-time
// constant folds where it is used, leaving nothing to read through and nothing
// to observe.
using ValueTarget =
    std::variant<RoutedValueRef, NamespaceValueRef, StaticPropertyRef>;

}  // namespace lyra::hir
