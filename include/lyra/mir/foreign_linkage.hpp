#pragma once

#include <string>
#include <variant>

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

// The unit that owns the callable defines the symbol, and that callable carries
// the prototype the name publishes, so nothing more is stated here.
struct UnitSymbolDefinition {};

// The subroutine behind the name is compiled once per specialization of its
// declaring scope, so the entries belong to the scopes rather than to any one
// unit and no single callable's signature is the published one. The design as a
// whole defines the symbol once for all of them, over the machine function type
// stated here.
struct PerScopeEntryDefinition {
  TypeId signature;
};

// Where the definition of a foreign name the design supplies lives. A name the
// design only declares is defined by the foreign side and takes part in no
// unit's surface, so it is not one of these.
using ForeignDefinition =
    std::variant<UnitSymbolDefinition, PerScopeEntryDefinition>;

// One foreign name this unit takes part in (LRM 35). A unit knows its own
// foreign surface as it is built, so it states it here rather than leaving a
// program-level consumer to search the unit's classes for linkage.
struct ForeignSymbol {
  ForeignLinkage linkage;
  ForeignDefinition definition;
};

}  // namespace lyra::mir
