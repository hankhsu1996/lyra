#pragma once

#include <string>

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

}  // namespace lyra::mir
