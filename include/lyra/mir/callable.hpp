#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/dpi_abi.hpp"

namespace lyra::mir {

// Whether a callable is part of the object's externally callable surface or an
// internal mechanism. A class instance method (LRM 8.6) is callable through a
// handle, so it is public; a scope's processes, lifecycle hooks, and helper
// subroutines are reached only by the owning runtime or by the scope's own
// bodies, so they are internal. A backend reads which access a callable has
// from here rather than inferring it from the object's base or the callable's
// role.
enum class CallableVisibility : std::uint8_t { kPublic, kInternal };

// One parameter of an external callable's ABI projection (LRM 35.5.6): the SV
// type the boundary marshals from, the C ABI carrier it crosses as, and its
// direction (LRM 35.5.1.2). All three are fixed once at HIR-to-MIR and read by
// a backend's marshaling, never re-derived from the type. The carrier decides
// how the value crosses -- a scalar in a register, a canonical vector by
// pointer to a buffer -- and the direction decides whether the boundary copies
// it in, back, or both.
struct ForeignParam {
  TypeId sv_type;
  support::DpiCarrier carrier;
  support::DpiDirection direction;
};

// The external implementation form of a callable: a foreign linkage name plus
// the pure property, with no SV body. A DPI-C import is realized as one
// (LRM 35.4). `is_pure` marks an import the LRM lets the simulator treat as
// side-effect-free (LRM 35.5.4). The source language and calling convention are
// implicitly C, the only foreign linkage today; a second linkage adds them
// here.
struct ExternalSymbol {
  std::string foreign_name;
  bool is_pure;
};

// A callable realized as a foreign symbol (LRM 35.4): a DPI-C import. The
// signature is the ABI projection -- each parameter's SV type and carrier, and
// the return SV type and carrier. A function result is restricted to a small
// value (LRM 35.5.5), so the return carrier is always a by-value scalar
// (`DpiScalarAbi`, not the full carrier variant, which makes a vector return
// unrepresentable). The SV semantic signature stays on the frontend where type
// checking ran; this carries only what marshaling and linkage need.
// `is_task` marks a task rather than a function (LRM 35.5): a task carries no
// SV return value, its call is a suspension point the caller awaits, and its
// foreign symbol returns the DPI disable-acknowledgment `int` (LRM 35.8) that
// the call discards. A `void`-returning function also has no return value, so
// the task distinction cannot be recovered from the return alone.
struct ExternalCallable {
  std::vector<ForeignParam> params;
  TypeId ret_sv_type;
  support::DpiScalarAbi ret_abi;
  bool is_task;
  ExternalSymbol external;
};

// A callable realized as an ordinary SV body. Its signature and body are the
// `CallableCode`: `self` is `code.params[0]` for an instance method (LRM 8.6)
// and simply absent for a receiver-less one (a package function, a static class
// method, LRM 26.3). The result type carries the call protocol and completion
// payload; a backend reads task-versus-function from it, not a side enum.
struct InternalCallable {
  CallableCode code;
};

// A named callable a class or a package namespace owns. Every SystemVerilog
// function and task, every process body, every synthesized lifecycle body, and
// every DPI-C import is this one concept, distinguished only by its
// implementation form (an SV body or a foreign symbol), whether its signature
// carries a receiver, and how a referencing site reaches it -- a direct call, a
// constructor-time process registration, an engine-dispatched lifecycle hook.
// The receiver is not a kind of callable: an instance method carries `self` as
// its first parameter, a static callable omits it. `virtual_dispatch`, when
// present, states this callable's role in the class's dispatch table (LRM 8.20)
// -- introducing a new slot or overriding an ancestor's -- so a backend renders
// the marker off stated structure, never re-deriving virtualness by name; it is
// absent for a direct-only callable (every static callable and DPI import).
struct CallableDecl {
  std::string name;
  std::variant<InternalCallable, ExternalCallable> impl;
  std::optional<VirtualDispatchRole> virtual_dispatch;
  CallableVisibility visibility;
};

// A named class-owned callable whose identity is a plain function pointer the
// runtime library holds and calls back through -- the shape a
// `void (*)(RuntimeScopeBase*)` lifecycle hook requires. Structurally a
// distinct callable species from `CallableDecl`: its receiver is an explicit
// parameter (never bound implicitly), it participates in no dispatch table, and
// it is reached only as a code address, never through a `CallExpr`. A backend
// renders it in the target language's function-pointer-compatible form, which
// is not the form an instance method takes.
struct AbiAdapter {
  std::string name;
  CallableCode code;
};

}  // namespace lyra::mir
