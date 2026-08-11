#pragma once

#include <cstdint>
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

// Where the definition of a foreign name lives. A name the design only declares
// is defined by the foreign side; one the design supplies is defined either as
// a linked symbol of the unit's own namespace, or -- when the subroutine behind
// it is compiled once per specialization of its declaring scope -- once for the
// whole program, dispatching to the entry each such scope publishes.
enum class ForeignDefinition : std::uint8_t {
  kForeignSide,
  kUnitSymbol,
  kPerScopeEntry,
};

// One foreign name this unit takes part in (LRM 35). A unit knows its own
// foreign surface as it is built, so it states it here rather than leaving a
// program-level consumer to search the unit's classes for linkage.
//
// `signature` is the machine function type the program-global symbol publishes.
// A symbol the unit itself defines carries that prototype on its own callable
// and needs nothing here; one defined over per-scope entries does not, because
// the entries belong to the scopes rather than to any unit, so the surface is
// where the prototype is stated for it.
struct ForeignSymbol {
  ForeignLinkage linkage;
  TypeId signature;
  ForeignDefinition definition = ForeignDefinition::kForeignSide;
};

}  // namespace lyra::mir
