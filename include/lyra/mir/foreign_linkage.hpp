#pragma once

#include <cstdint>
#include <string>

#include "lyra/base/internal_error.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {

// How many artifacts of the program may carry a definition of one foreign
// symbol. It governs a definition, so it is read where one is being emitted and
// nowhere else: a bodyless callable and a linkage naming an entry a scope
// publishes both carry it unread.
enum class ForeignDefinition : std::uint8_t {
  // One artifact defines the symbol -- the unit whose own namespace declares
  // the subroutine behind it, which is the only unit that can.
  kSingle,
  // Every unit declaring a scope that exports the name defines it, all of them
  // the same way, and whoever assembles the program keeps one. LRM 35.4 allows
  // one linkage name across several scopes with equivalent signatures, and
  // those scopes may sit in different units, so the symbol belongs to no unit
  // and what reaches one definition is a merge rule rather than an owner.
  kEachDeclarer,
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
// is the callable's own signature, so nothing is restated here. The source
// language and calling convention are implicitly C, the only foreign linkage
// today; a second linkage adds them here.
struct ForeignLinkage {
  std::string foreign_name;
  ForeignDefinition definition = ForeignDefinition::kSingle;
};

// Whether an artifact carrying this definition must expect others to carry it
// too. Every consumer asking that asks it through here, so the set is read in
// one place and gaining an alternative is a question this has to answer again
// rather than one each reader answers by omission.
[[nodiscard]] inline auto ExpectsOtherDefinitions(const ForeignLinkage& linkage)
    -> bool {
  switch (linkage.definition) {
    case ForeignDefinition::kSingle:
      return false;
    case ForeignDefinition::kEachDeclarer:
      return true;
  }
  throw InternalError("mir: unknown foreign definition");
}

// A foreign name whose entry sits on a scope rather than in a unit's namespace
// (LRM 35.5.3). The subroutine behind it is compiled once per specialization of
// that scope, so no one of those entries is the published symbol and no single
// callable's signature is the published prototype; the unit declaring the scope
// defines the symbol over the machine function type stated here, and does so
// knowing another unit may define the same one.
//
// A name a unit's own namespace owns needs no entry of this kind: its callable
// is the symbol and carries the prototype. So is a name the design only
// declares, which the foreign side defines.
struct ForeignScopeEntry {
  ForeignLinkage linkage;
  TypeId signature;
};

}  // namespace lyra::mir
