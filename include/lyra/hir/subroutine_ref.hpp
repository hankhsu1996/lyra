#pragma once

#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/hir/array_iterator_method.hpp"
#include "lyra/hir/callee_interface.hpp"
#include "lyra/hir/class_id.hpp"
#include "lyra/hir/class_ref.hpp"
#include "lyra/hir/enum_method.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/foreign_import_id.hpp"
#include "lyra/hir/interface_member_access.hpp"
#include "lyra/hir/published_callable.hpp"
#include "lyra/hir/randomization_hook.hpp"
#include "lyra/hir/sampled_history.hpp"
#include "lyra/hir/scope_class.hpp"
#include "lyra/hir/structural_hops.hpp"
#include "lyra/hir/subroutine_id.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::hir {

// Calls a subroutine the caller's own scope declares, on that scope's object.
// `subroutine` is that scope's own identity for the callee, a registry
// position. One an enclosing scope or a generate block declares is called
// through what that scope published (LRM 23.9, 27.4, 23.6), since which body
// each is, is its object's.
struct StructuralSubroutineRef {
  StructuralSubroutineId subroutine;

  auto operator==(const StructuralSubroutineRef&) const -> bool = default;
};

// Calls a DPI-C import (LRM 35.4). `id` names the unit's own record of the
// import; the import's foreign symbol is program-global, so the call reaches it
// without naming whichever unit spelled the declaration, and a declaration in a
// package or at `$unit` scope is called exactly as one in this unit's own scope
// is.
//
// The instantiated scope the declaration sits in, so many enclosing levels up
// from the call site, is the one thing about an import that is not global: a
// `context` import observes it for the duration of its foreign call (LRM
// 35.5.3). It is absent when the declaration sits in a namespace that is never
// instantiated -- a package or `$unit` scope -- and the import then observes no
// scope, so only a receiver-less export is directly reachable from it and any
// other needs `svSetScope`.
struct ForeignImportRef {
  ForeignImportId id{};
  std::optional<StructuralHops> declaring_scope;

  auto operator==(const ForeignImportRef&) const -> bool = default;
};

// Which object an instance-method call runs against (LRM 8.6, 8.11, 8.15).
// The arms are the answers, not the source spellings, so two spellings that
// name one object share one arm.
//
// - `HandleReceiver` -- LRM 8.6 qualified `h.foo()`: the source supplied a
//   class-handle expression the call dispatches through, and the call obeys
//   the callee's own virtual role.
// - `SelfReceiver` -- the object the enclosing method was invoked on, which
//   an unqualified `foo()` names (LRM 8.6) and which `this.foo()` names
//   explicitly (LRM 8.11). The call obeys the callee's virtual role.
// - `SuperReceiver` -- LRM 8.15 `super.foo()`: the receiver is still the
//   enclosing method's self, but the source demands the base's
//   implementation and the call must skip dynamic dispatch regardless of
//   whether the target is virtual.
//
// The three arms are structurally disjoint because "receiver source" and
// "dispatch qualifier" are not independent axes -- a super-qualified call is
// never through an explicit handle, so encoding them as a receiver-optional
// plus a super-flag would admit an invalid state.
struct HandleReceiver {
  ExprId expr;

  auto operator==(const HandleReceiver&) const -> bool = default;
};
struct SelfReceiver {
  auto operator==(const SelfReceiver&) const -> bool = default;
};
struct SuperReceiver {
  auto operator==(const SuperReceiver&) const -> bool = default;
};

using MethodReceiver =
    std::variant<HandleReceiver, SelfReceiver, SuperReceiver>;

// A hook `randomize()` runs that class `cls` has without declaring it (LRM
// 18.6.2), as the front end gives every class one of each it does not declare:
// `cls` is the class the call names, the one a handle is of, the class of the
// method writing the call, or that class's base through `super`. Which body
// runs follows from that class, since an interface class's hooks are virtual
// (LRM 8.26.9) and a class's are not (LRM 18.6.2).
struct BuiltinHookCallee {
  ClassId cls;
  RandomizationHook hook{};

  auto operator==(const BuiltinHookCallee&) const -> bool = default;
};

// The method an instance-method call names: one a class declares, or a hook a
// class has without declaring it.
using InstanceMethodCallee = std::variant<ClassMethodTarget, BuiltinHookCallee>;

// Calls an instance method (LRM 8.6). `receiver` states which object the call
// runs against, whichever source form named it.
struct MethodCallRef {
  MethodReceiver receiver;
  InstanceMethodCallee callee;

  auto operator==(const MethodCallRef&) const -> bool = default;
};

// Calls a `$xxx` system subroutine. The id resolves through
// `support::LookupSystemSubroutine` to the descriptor that drives lowering.
struct SystemSubroutineRef {
  support::SystemSubroutineId id;

  auto operator==(const SystemSubroutineRef&) const -> bool = default;
};

// Calls a built-in runtime method (LRM 6.16 string, 7.9 associative, 7.10
// queue, 7.12 unpacked-array shared family, 15.5 named event, 9.7 process).
// The id is the flat closed namespace `support::BuiltinFn`, shared with MIR.
// `receiver` is the object the entry acts on, absent for one that acts on
// none: a factory answering with the value it builds, and a method of a library
// class that names the running process rather than one it is handed.
struct BuiltinMethodRef {
  support::BuiltinFn method{};
  std::optional<ExprId> receiver;

  auto operator==(const BuiltinMethodRef&) const -> bool = default;
};

// Calls a method LRM 6.19.5 defines on an enumerated type. The enumeration is
// the type of the leading argument, the same way every other built-in method
// call carries the type it dispatches on; what separates the two is that no
// runtime library declares these, so nothing below this layer names one.
struct EnumMethodRef {
  EnumMethod method;

  auto operator==(const EnumMethodRef&) const -> bool = default;
};

// Calls a method LRM 7.12 defines on an unpacked array that evaluates an
// expression at each entry. The array is the leading argument; `with_clause`
// is the expression, absent where the call writes none and the element itself
// is asked (LRM 7.12.1). As with an enumeration's methods, no runtime library
// declares these.
struct ArrayIteratorMethodRef {
  ArrayIteratorMethod method{};
  std::optional<WithClause> with_clause;

  auto operator==(const ArrayIteratorMethodRef&) const -> bool = default;
};

// The two shapes a sampled value function that reaches across the ticks of a
// clocking event takes (LRM 16.9.3). Both name a history, because which event's
// ticks the call counts is settled where the source is, by that clause's
// ordered rules, and none of that survives into a lower layer.
//
// `$sampled` is neither. It names no event, so it reads the cell directly and
// stays an ordinary system subroutine call.

// `$past`: the value one tick of that event settled. The operand is not an
// argument of this call -- it is the history's subject, evaluated at the tick
// rather than here -- but how far back to reach is, and it is the call's one
// argument: the expression the source wrote there, or the 1 it defaults to
// (LRM 16.9.3). That clause requires the expression to settle before the
// program runs, which says its value is known and not that this has to hold it:
// a history answers by counting back through entries it already has, so a
// different distance is the same history reached at another offset.
struct PastValueRef {
  SampledHistoryId history;

  auto operator==(const PastValueRef&) const -> bool = default;
};

// `$rose`, `$fell`, `$stable`, `$changed`: a comparison between the sampled
// value of the current time step and the one the most recent strictly prior
// tick settled. The current side is still evaluated where the call stands, so
// this one keeps the operand as its argument.
struct ValueChangeRef {
  SampledHistoryId history;
  support::ValueChangeReading reading = support::ValueChangeReading::kRoseToOne;

  auto operator==(const ValueChangeRef&) const -> bool = default;
};

// Calls a subroutine of a namespace unit -- a package function or task (LRM
// 26.3) -- by the unit and the subroutine's name, whichever unit's namespace
// declares it, the declaring unit's own bodies included: a namespace has no
// instance to reach it through, so it is never an enclosing-scope hop, and the
// name is resolved at link time, the way an instantiated child names its unit.
// `interface` is what the call passes and awaits, read off the declaring
// unit's declarations where this is minted.
struct NamespaceSubroutineRef {
  std::string unit_name;
  std::string subroutine_name;
  CalleeInterface interface;

  auto operator==(const NamespaceSubroutineRef&) const -> bool = default;
};

// The object a call on another unit's instance is made on: one a route reaches,
// which elaboration bound, or one reached through a virtual interface, which is
// whatever the handle holds when the call runs (LRM 25.9) and fails there if
// it holds nothing.
using UnitObjectReceiver =
    std::variant<RoutedObjectRef, InterfaceInstanceAccessExpr>;

// Calls a subroutine a unit declares in one of its scopes -- its own body or a
// generate block in it, a generate block of this unit included -- enabled on
// one object of that scope (LRM 23.6, 25.7, 27.4): reached through a port
// bound to an instance, through a hierarchical name, or through a virtual
// interface holding one. `receiver` is how that object is reached, and it is
// passed as the callable's first argument. `scope_class` is the class the
// declaring unit published the scope as, and `callable` the entry the name
// resolved to, so what the call passes and what it awaits come from what was
// published rather than from the declaration behind it.
struct PublishedScopeMethodRef {
  UnitObjectReceiver receiver;
  ScopeClassId scope_class;
  PublishedCallableId callable;

  auto operator==(const PublishedScopeMethodRef&) const -> bool = default;
};

// Calls a static class method (LRM 8.10). Distinct from `MethodCallRef`
// because a static method has no receiver -- neither an explicit handle, the
// enclosing method's own object, nor a super qualifier -- and encoding it as a
// receiver-optional variant of `MethodCallRef` would admit an invalid state.
// Under inheritance, `Derived::inherited_static()` still names the base -- the
// method lives on the base's arena -- mirroring the owner-qualified rule for
// inherited instance access.
// `declaring_instance` is how this body reaches the instance of the scope that
// declares the class. A class a structural scope declares is a type of that
// scope's instance (LRM 6.22), so what it keeps for itself is that instance's;
// a receiver-less method reaching any of it is handed the instance, since it
// has no object to reach one through. Present exactly where the class takes
// the instance, which is the class's own declaration to say.
struct StaticMethodCallRef {
  ClassMethodTarget callee;
  std::optional<DeclaringInstanceReach> declaring_instance;

  auto operator==(const StaticMethodCallRef&) const -> bool = default;
};

using SubroutineRef = std::variant<
    StructuralSubroutineRef, MethodCallRef, StaticMethodCallRef,
    SystemSubroutineRef, BuiltinMethodRef, EnumMethodRef,
    ArrayIteratorMethodRef, PastValueRef, ValueChangeRef, ForeignImportRef,
    NamespaceSubroutineRef, PublishedScopeMethodRef>;

}  // namespace lyra::hir
