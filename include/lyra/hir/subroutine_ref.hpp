#pragma once

#include <compare>
#include <cstdint>
#include <optional>
#include <variant>
#include <vector>

#include "lyra/hir/array_iterator_method.hpp"
#include "lyra/hir/class_id.hpp"
#include "lyra/hir/class_ref.hpp"
#include "lyra/hir/enum_method.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/foreign_import_id.hpp"
#include "lyra/hir/interface_member_access.hpp"
#include "lyra/hir/namespace.hpp"
#include "lyra/hir/published_callable.hpp"
#include "lyra/hir/randomization_hook.hpp"
#include "lyra/hir/scope_class_id.hpp"
#include "lyra/hir/structural_hops.hpp"
#include "lyra/hir/subroutine_id.hpp"
#include "lyra/hir/timing.hpp"
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

// How an invocation finds the method that runs from the one it names: its
// invocation mode, as Java, whose class model this follows, names it (JLS
// 15.12.3).
enum class InvocationMode : std::uint8_t {
  // The named method's own role decides: the latest override where it is
  // virtual, the named method where it is not (LRM 8.20).
  kVirtual,
  // `super.m()`, or `C::m()` with `C` a superclass of the object: the call
  // names the class whose method runs, and overriding is not allowed (LRM
  // 8.15, 8.23), as Java's `super` mode covers both `super.m()` and
  // `T.super.m()`.
  kSuper,
};

// A hook `randomize()` runs that class `cls` has without declaring it (LRM
// 18.6.2), as the front end gives every class one of each it does not declare:
// `cls` is the class the call names, the one a handle is of, the class of the
// method writing the call, or that class's base through `super`. Which body
// runs follows from that class: a class's hooks are not virtual (LRM 18.6.2),
// and an interface class's run the object's own class's, the reading Lyra
// takes where LRM 8.26.9 calls them virtual.
struct BuiltinHookCallee {
  ClassId cls;
  RandomizationHook hook{};

  auto operator==(const BuiltinHookCallee&) const -> bool = default;
};

// The method an instance-method call names: one a class declares, or a hook a
// class has without declaring it.
using InstanceMethodCallee = std::variant<ClassMethodTarget, BuiltinHookCallee>;

// Calls an instance method (LRM 8.6) on the object `receiver` evaluates to; a
// call naming no object runs on `this` (LRM 8.11).
struct MethodCallRef {
  ExprId receiver;
  InstanceMethodCallee callee;
  InvocationMode mode{};

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
// clocking event takes (LRM 16.9.3). Each holds the event whose ticks it
// counts, because which event that is was settled where the source is, by that
// clause's ordered rules, and none of that survives into a lower layer; where
// `$past` was given a gate, the event already carries it as the LRM 9.4.2.3
// qualifier, since the gate decides which ticks count rather than what is
// read. The operand is the call's first argument, evaluated where the call
// stands as the sampled value of an earlier tick (LRM 16.5.1); what keeps the
// values of those ticks is worked out from the call itself, which is that
// history's identity.
//
// `$sampled` is neither. It names no event, so it reads the cell directly and
// stays an ordinary system subroutine call.

// `$past`: the operand, then the tick count -- the expression the source wrote
// there or the 1 it defaults to, settled before the program runs.
struct PastValueRef {
  EventControl clock;

  auto operator==(const PastValueRef&) const -> bool = default;
};

// `$rose`, `$fell`, `$stable`, `$changed`: the operand's sampled value in the
// current time step against the one of the most recent strictly prior tick.
// The operand is its one argument.
struct ValueChangeRef {
  EventControl clock;
  support::ValueChangeReading reading = support::ValueChangeReading::kRoseToOne;

  auto operator==(const ValueChangeRef&) const -> bool = default;
};

// Calls a subroutine a namespace declares -- a package function or task (LRM
// 26.3) -- by the namespace and the subroutine's position in what it declares,
// the namespace's own bodies included: a namespace has no instance to reach it
// through, so it is never an enclosing-scope hop. What the call passes and
// awaits is the namespace's declaration of it.
struct NamespaceSubroutineRef {
  NamespaceId owner;
  NamespaceSubroutineId subroutine;

  auto operator<=>(const NamespaceSubroutineRef&) const
      -> std::strong_ordering = default;
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

// The scope an import is declared in, as one call reaches it, with the
// functions evaluating the defaults the call leaves to it (LRM 13.5.3, 35.5.4)
// named as that scope names its subroutines. Each `defaults` holds, at the
// position of each formal the call omitted, that formal's evaluator.
//
// The call's own scope, so many enclosing levels up, whose subroutines this
// unit declares.
struct OwnDeclaringScope {
  StructuralHops hops;
  std::vector<std::optional<StructuralSubroutineId>> defaults;

  auto operator==(const OwnDeclaringScope&) const -> bool = default;
};

// Another instance's object, which elaboration bound, whose subroutines its
// unit published as the class `scope_class`.
struct PublishedDeclaringScope {
  RoutedObjectRef receiver;
  ScopeClassId scope_class;
  std::vector<std::optional<PublishedCallableId>> defaults;

  auto operator==(const PublishedDeclaringScope&) const -> bool = default;
};

// A namespace -- a package or `$unit` scope -- which is never instantiated, so
// no object stands for it, and whose evaluators a call names by their
// positions in what it declares.
struct NamespaceDeclaringScope {
  NamespaceId owner;
  std::vector<std::optional<NamespaceSubroutineId>> defaults;

  auto operator==(const NamespaceDeclaringScope&) const -> bool = default;
};

using ImportDeclaringScope = std::variant<
    OwnDeclaringScope, PublishedDeclaringScope, NamespaceDeclaringScope>;

// Calls a DPI-C import (LRM 35.4). `id` names the unit's own record of the
// import; the import's foreign symbol is program-global, so the call reaches it
// without naming whichever unit spelled the declaration, and a declaration in a
// package or at `$unit` scope is called exactly as one in this unit's own scope
// is.
//
// Declaring an import is defining a subroutine of its scope (LRM 35.5.4), so
// that scope is the one thing about it that is not global: a `context` import
// observes it for the duration of its foreign call (LRM 35.5.3), and it
// evaluates the defaults the call leaves to it. `declaring_scope` is absent
// where the call needs neither of it.
struct ForeignImportRef {
  ForeignImportId id{};
  std::optional<ImportDeclaringScope> declaring_scope;

  auto operator==(const ForeignImportRef&) const -> bool = default;
};

// Calls a static class method (LRM 8.10), which runs on no object, so the
// reference holds none.
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
