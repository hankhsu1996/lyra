#pragma once

#include <optional>
#include <string>
#include <variant>

#include "lyra/hir/class_ref.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/external_callee.hpp"
#include "lyra/hir/external_unit_object.hpp"
#include "lyra/hir/foreign_import_id.hpp"
#include "lyra/hir/published_callable.hpp"
#include "lyra/hir/structural_hops.hpp"
#include "lyra/hir/subroutine_id.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/imported_runtime_class.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::hir {

// Calls a structural subroutine declared in the unit (or one of its
// enclosing scopes, reached through `hops`).
struct StructuralSubroutineRef {
  StructuralHops hops;
  StructuralSubroutineId subroutine;
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
};

// The receiver form of an instance-method call (LRM 8.6, 8.15). The three
// SV source spellings each map to one arm; none combines with another.
//
// - `HandleReceiver` -- LRM 8.6 qualified `h.foo()`: the source supplied a
//   class-handle expression the call dispatches through, and the call obeys
//   the callee's own virtual role.
// - `ImplicitSelfReceiver` -- LRM 8.6 unqualified `foo()` from inside a
//   class method: the receiver is the enclosing method's own self, and the
//   call obeys the callee's virtual role.
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
};
struct ImplicitSelfReceiver {};
struct SuperReceiver {};

using MethodReceiver =
    std::variant<HandleReceiver, ImplicitSelfReceiver, SuperReceiver>;

// Calls a class method whose declaring class is in another compilation unit.
// Beside naming the callee it carries the facts this unit cannot look up about
// one: whether the method is virtual (LRM 8.20), which routes the call between
// dynamic and static dispatch; whether it is type-associated (LRM 8.10), which
// is whether it takes a receiver at all; and its interface, which shapes the
// arguments the call passes and the completion it consumes. All are read from
// the frontend's view of the callee where this callee is minted.
struct ExternalMethodCallee {
  ExternalClassMethodTarget target;
  bool is_virtual = false;
  bool is_static = false;
  ExternalCalleeInterface interface;
};

// The method a call reaches. Intra-unit it is a slot in a class's own method
// arena, and everything the call needs follows from the declaration that slot
// resolves to; cross-unit there is no such declaration to reach, so the callee
// carries what the call would have read off one.
using MethodCallee = std::variant<LocalClassMethodTarget, ExternalMethodCallee>;

// Calls an instance method (LRM 8.6). `receiver` states which of the three
// LRM-defined source forms reached this call site.
struct MethodCallRef {
  MethodReceiver receiver;
  MethodCallee callee;
};

// Calls a `$xxx` system subroutine. The id resolves through
// `support::LookupSystemSubroutine` to the descriptor that drives lowering.
struct SystemSubroutineRef {
  support::SystemSubroutineId id;
};

// Calls a built-in runtime method (LRM 6.16 string, 6.19.5 enum, 7.9
// associative, 7.10 queue, 7.12 unpacked-array shared family, 15.5 named
// event). The id is the flat closed namespace `support::BuiltinFn`, shared
// with MIR.
struct BuiltinMethodRef {
  support::BuiltinFn method;
};

// Calls a method the runtime library provides for an imported class (LRM 9.7
// `process`). A bodyless external callable named by its library identity; the
// receiver is present for an instance method and absent for a static one.
struct ImportedMethodRef {
  support::ImportedRuntimeMethod method =
      support::ImportedRuntimeMethod::kProcessSelf;
  std::optional<ExprId> receiver = std::nullopt;
};

// Calls a subroutine that belongs to another compilation unit -- a package
// function or task (LRM 26.3), reached by name. The target lives outside this
// unit, so it carries no unit-local id: the referring unit names the package
// and the subroutine by name and resolves against that interface at link time,
// the way an instantiated child names its unit, and never through an
// enclosing-scope hop within this unit.
struct ExternalUnitSubroutineRef {
  std::string unit_name;
  std::string subroutine_name;
  ExternalCalleeInterface interface;
};

// Calls a subroutine another compilation unit declares in its own body, enabled
// on one instance of that unit (LRM 25.7): an interface's task or function,
// reached through a port bound to the instance or through a hierarchical name
// that reaches it. `receiver` is the route to that object, sealed like every
// other reference across an instance boundary and passed as the callable's
// first argument. `object` is this unit's record of what the other unit
// published, and `callable` the entry the name resolved to, so what the call
// passes and what it awaits come from the promise rather than from the
// declaration behind it.
struct ExternalUnitMethodRef {
  RoutedRef receiver;
  ExternalUnitObjectId object;
  PublishedCallableId callable;
};

// Calls a static class method (LRM 8.10). Distinct from `MethodCallRef`
// because a static method has no receiver -- neither an explicit handle, an
// implicit self, nor a super qualifier -- and encoding it as a receiver-
// optional variant of `MethodCallRef` would admit an invalid state. Under
// inheritance, `Derived::inherited_static()` still names the base -- the method
// lives on the base's arena -- mirroring the owner-qualified rule for inherited
// instance access.
struct StaticMethodCallRef {
  MethodCallee callee;
};

using SubroutineRef = std::variant<
    StructuralSubroutineRef, MethodCallRef, StaticMethodCallRef,
    SystemSubroutineRef, BuiltinMethodRef, ForeignImportRef, ImportedMethodRef,
    ExternalUnitSubroutineRef, ExternalUnitMethodRef>;

}  // namespace lyra::hir
