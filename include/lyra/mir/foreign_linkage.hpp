#pragma once

#include <optional>
#include <string>
#include <vector>

#include "lyra/mir/type_id.hpp"
#include "lyra/support/dpi_abi.hpp"

namespace lyra::mir {

// One formal of a foreign signature's SV-side projection (LRM 35.5.6): the SV
// type the boundary marshals from, the C ABI carrier it crosses as, and its
// direction (LRM 35.5.1.2). Fixed once at HIR-to-MIR and read where a call
// lowers its boundary, never re-derived from the type. The carrier decides how
// the value crosses -- a scalar in a register, a canonical vector by pointer to
// a buffer -- and the direction decides whether the boundary copies it in,
// back, or both.
struct ForeignParam {
  TypeId sv_type;
  support::DpiCarrier carrier;
  support::DpiDirection direction;
};

// How a foreign signature projects onto SV values, which is what a call site
// needs to build its boundary. A function result is restricted to a small value
// (LRM 35.5.5), so the return is always a by-value scalar, which makes a vector
// return unrepresentable. `is_task` marks a task rather than a function (LRM
// 35.5): its call is a suspension point the caller awaits, and its foreign
// symbol returns the disable-acknowledgment int (LRM 35.8) that the call
// discards. A `void`-returning function also has no return value, so the task
// distinction cannot be recovered from the return alone.
struct ForeignMarshal {
  std::vector<ForeignParam> params;
  TypeId ret_sv_type;
  support::DpiScalarAbi ret_abi;
  bool is_task;
};

// The C linkage contract of a callable that crosses the DPI-C boundary, in
// either direction. The two directions carry one shape because they are one
// boundary: a bodyless callable is an `import "DPI-C"` whose definition the
// user's C provides (LRM 35.4), a bodied one is the entry point of an
// `export "DPI-C"` that the user's C calls (LRM 35.7). Which of the two a
// callable is follows from whether it has a body; nothing here restates it.
//
// A foreign name is program-global and lives in its own name space, distinct
// from any compilation-unit scope (LRM 35.4, 35.7), and all declarations
// sharing one name must agree on one prototype (LRM 35.5.4) -- that prototype
// is the callable's own signature, so nothing is restated here. `is_pure` marks
// an import the simulator may treat as side-effect-free (LRM 35.5.4).
// `is_context` marks a callable that observes the instantiated scope of its
// declaration and may reach SV state (LRM 35.5.3); every export is one (LRM
// 35.7). The source language and calling convention are implicitly C, the only
// foreign linkage today; a second linkage adds them here.
struct ForeignLinkage {
  std::string foreign_name;
  bool is_pure;
  bool is_context;
  // Present for the direction whose calls this program lowers: an import, where
  // every call site builds the boundary from this projection. An export is
  // called only from foreign code, and its entry point's body already carries
  // the marshaling as ordinary statements, so it needs none.
  std::optional<ForeignMarshal> marshal;
};

}  // namespace lyra::mir
