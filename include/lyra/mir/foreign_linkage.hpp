#pragma once

#include <string>

#include "lyra/mir/type_id.hpp"

namespace lyra::mir {

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
// is the callable's own signature, so nothing is restated here. The source
// language and calling convention are implicitly C, the only foreign linkage
// today; a second linkage adds them here.
struct ForeignLinkage {
  std::string foreign_name;
};

// A foreign name whose entry sits on a scope rather than in a unit's namespace
// (LRM 35.5.3). The subroutine behind it is compiled once per specialization of
// that scope, so no one of those entries is the published symbol and no single
// callable's signature is the published prototype; the program defines the
// symbol once for all of them, over the machine function type stated here.
//
// A name a unit's own namespace owns needs no entry of this kind: its callable
// is the symbol and carries the prototype. So is a name the design only
// declares, which the foreign side defines.
struct ForeignScopeEntry {
  ForeignLinkage linkage;
  TypeId signature;
};

}  // namespace lyra::mir
