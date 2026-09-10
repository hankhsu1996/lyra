#pragma once

#include <optional>
#include <string>
#include <variant>

#include "lyra/hir/class_ref.hpp"
#include "lyra/hir/enum_method.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/external_callee.hpp"
#include "lyra/hir/external_unit_object.hpp"
#include "lyra/hir/foreign_import_id.hpp"
#include "lyra/hir/published_callable.hpp"
#include "lyra/hir/sampled_history.hpp"
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
};
struct SelfReceiver {};
struct SuperReceiver {};

using MethodReceiver =
    std::variant<HandleReceiver, SelfReceiver, SuperReceiver>;

// Calls a class method whose declaring class is in another compilation unit.
// Beside naming the callee it carries the facts this unit cannot look up about
// one: the behavior the method answers, absent for a method that answers none
// (LRM 8.20), and its interface, which shapes the arguments the call passes and
// the completion it consumes. Both are read from the frontend's view of the
// callee where this callee is minted. A method that answers a behavior can be
// dispatched on; whether a given call is, is that call's own question, since
// naming the base's implementation demands it whatever the callee answers
// (LRM 8.15). Whether the method is type-associated (LRM 8.10) is not among
// them: a call to one is a different reference, which is where that shows.
struct ExternalMethodCallee {
  ExternalClassMethodTarget target;
  std::optional<ExternalDispatchSlot> slot;
  ExternalCalleeInterface interface;
};

// The method a call reaches. Intra-unit it is a slot in a class's own method
// arena, and everything the call needs follows from the declaration that slot
// resolves to; cross-unit there is no such declaration to reach, so the callee
// carries what the call would have read off one.
using MethodCallee = std::variant<LocalClassMethodTarget, ExternalMethodCallee>;

// Calls an instance method (LRM 8.6). `receiver` states which object the call
// runs against, whichever source form named it.
struct MethodCallRef {
  MethodReceiver receiver;
  MethodCallee callee;
};

// Calls a `$xxx` system subroutine. The id resolves through
// `support::LookupSystemSubroutine` to the descriptor that drives lowering.
struct SystemSubroutineRef {
  support::SystemSubroutineId id;
};

// Calls a built-in runtime method (LRM 6.16 string, 7.9 associative, 7.10
// queue, 7.12 unpacked-array shared family, 15.5 named event). The id is the
// flat closed namespace `support::BuiltinFn`, shared with MIR.
struct BuiltinMethodRef {
  support::BuiltinFn method;
};

// Calls a method LRM 6.19.5 defines on an enumerated type. The enumeration is
// the type of the leading argument, the same way every other built-in method
// call carries the type it dispatches on; what separates the two is that no
// runtime library declares these, so nothing below this layer names one.
struct EnumMethodRef {
  EnumMethod method;
};

// The two shapes a sampled value function that reaches across the ticks of a
// clocking event takes (LRM 16.9.3). Both name a history, because which event's
// ticks the call counts is settled where the source is -- written at the call,
// inferred from the procedure (LRM 16.14.6), or taken from the scope's default
// clocking (LRM 14.12) -- and none of that survives into a lower layer.
//
// `$sampled` is neither. It names no event, so it reads the cell directly and
// stays an ordinary system subroutine call.

// `$past`: the value one tick of that event settled, and nothing of the time
// step the call stands in -- so it takes no argument. `ticks_back` is how far
// back it reaches, which the standard requires to be an elaboration-time
// constant and defaults to 1.
struct PastValueRef {
  SampledHistoryId history;
  std::uint32_t ticks_back = 1;
};

// `$rose`, `$fell`, `$stable`, `$changed`: a comparison between the sampled
// value of the current time step and the one the most recent strictly prior
// tick settled. The current side is still evaluated where the call stands, so
// this one keeps the operand as its argument.
struct ValueChangeRef {
  SampledHistoryId history;
  support::ValueChangeReading reading = support::ValueChangeReading::kRoseToOne;
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
// because a static method has no receiver -- neither an explicit handle, the
// enclosing method's own object, nor a super qualifier -- and encoding it as a
// receiver-optional variant of `MethodCallRef` would admit an invalid state.
// Under inheritance, `Derived::inherited_static()` still names the base -- the
// method lives on the base's arena -- mirroring the owner-qualified rule for
// inherited instance access.
// `declaring_scope_hops` is how far out of this body's own structural scope the
// scope that declares the class sits, present exactly where one does. A class a
// structural scope declares is a type of that scope's instance (LRM 6.22), so
// what it keeps for itself is that instance's; a receiver-less method reaching
// any of it is handed the instance, since it has no object to reach one
// through. Absent for a class a namespace unit declares, which no instance
// replicates.
struct StaticMethodCallRef {
  MethodCallee callee;
  std::optional<StructuralHops> declaring_scope_hops;
};

using SubroutineRef = std::variant<
    StructuralSubroutineRef, MethodCallRef, StaticMethodCallRef,
    SystemSubroutineRef, BuiltinMethodRef, EnumMethodRef, PastValueRef,
    ValueChangeRef, ForeignImportRef, ImportedMethodRef,
    ExternalUnitSubroutineRef, ExternalUnitMethodRef>;

}  // namespace lyra::hir
