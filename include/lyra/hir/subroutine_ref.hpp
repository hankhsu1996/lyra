#pragma once

#include <optional>
#include <string>
#include <variant>

#include "lyra/hir/class_id.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/foreign_import_id.hpp"
#include "lyra/hir/method_id.hpp"
#include "lyra/hir/structural_hops.hpp"
#include "lyra/hir/subroutine_id.hpp"
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

// Calls a DPI-C import (LRM 35.4) declared in the unit (or an enclosing scope,
// reached through `hops`). A bodyless external callable, resolved separately
// from a body-bearing structural subroutine.
struct ForeignImportRef {
  StructuralHops hops;
  ForeignImportId id;
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

// Calls an instance method (LRM 8.6). `class_id` names the declaring class
// and `method` is the method's declaration-order position in that class's HIR
// method arena. `receiver` states which of the three LRM-defined source
// forms reached this call site.
struct MethodCallRef {
  MethodReceiver receiver;
  ClassId class_id;
  MethodId method;
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
};

// Calls a static class method (LRM 8.10). Distinct from `MethodCallRef`
// because a static method has no receiver -- neither an explicit handle, an
// implicit self, nor a super qualifier -- and encoding it as a receiver-
// optional variant of `MethodCallRef` would admit an invalid state. `owner`
// names the class that declares the method; `method` is its position within
// that class's method arena. Under inheritance, `Derived::inherited_static()`
// still names the base as `owner` -- the method lives on the base's arena --
// mirroring the owner-qualified rule for inherited instance access.
struct StaticMethodCallRef {
  ClassId class_id;
  MethodId method;
};

using SubroutineRef = std::variant<
    StructuralSubroutineRef, MethodCallRef, StaticMethodCallRef,
    SystemSubroutineRef, BuiltinMethodRef, ForeignImportRef, ImportedMethodRef,
    ExternalUnitSubroutineRef>;

}  // namespace lyra::hir
