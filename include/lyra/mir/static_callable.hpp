#pragma once

#include <string>
#include <vector>

#include "lyra/mir/type_id.hpp"
#include "lyra/support/dpi_abi.hpp"

namespace lyra::mir {

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

// A class-level receiver-less associated callable, the callable peer of a
// static constant. It is called by name with no `self`, so it is a
// type-associated function, not an instance method, and lives in the class's
// static-callable namespace rather than the instance method arena. Its
// implementation form is an external foreign symbol today -- a DPI-C import; a
// bodied static (an SV class static method) is the second form the same home
// admits when that feature lands, at which point the implementation form
// becomes a variant.
//
// The signature is the ABI projection: each parameter's SV type and carrier,
// and the return SV type and carrier. A function result is restricted to a
// small value (LRM 35.5.5), so the return carrier is always a by-value scalar
// -- `DpiScalarAbi`, not the full carrier variant, which makes a vector return
// unrepresentable. The SV semantic signature stays on the frontend where type
// checking ran; MIR carries only what marshaling and linkage need.
//
// `is_task` marks a task rather than a function (LRM 35.5): a task carries no
// SV return value, its call is a suspension point the caller awaits, and its
// foreign symbol returns the DPI disable-acknowledgment `int` (LRM 35.8) that
// the call discards. A `void`-returning function also has no return value, so
// the task distinction cannot be recovered from the return alone.
struct StaticCallableDecl {
  std::string name;
  std::vector<ForeignParam> params;
  TypeId ret_sv_type;
  support::DpiScalarAbi ret_abi;
  bool is_task;
  ExternalSymbol external;
};

}  // namespace lyra::mir
