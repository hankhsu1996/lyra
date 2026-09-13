#pragma once

#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::compiler {

// What a namespace unit's storage bring-up owes the program.
struct NamespaceBringUp {
  // The units this namespace's value initializers read directly. LRM 26.2
  // fixes a barrier rather than an order -- every package variable's
  // declaration assignment happens before any initial or always procedure
  // starts, and the order among packages is left open -- so an order is
  // chosen rather than required, and choosing it from these reads is what
  // makes one run's output match the next.
  std::vector<std::string> initializer_unit_reads;
};

// The user's own C defines it: an `import "DPI-C"` (LRM 35.4).
struct DefinedByTheForeignSide {};

// The declaring unit owns the entry point, so its own artifact defines the
// symbol: an `export "DPI-C"` whose subroutine a namespace owns (LRM 35.7).
struct DefinedByTheUnit {};

// Only the program can own the symbol, so the design root defines it: an
// export whose subroutine is reached through a scope, compiled once per
// specialization of that scope, so no one of them is the published entry.
struct DefinedByTheProgram {};

using ForeignBody = std::variant<
    DefinedByTheForeignSide, DefinedByTheUnit, DefinedByTheProgram>;

// One foreign name this unit takes part in (LRM 35), with the prototype every
// declaration of that name must agree on (LRM 35.5.4). The name is
// program-global and lives in its own name space rather than any unit's scope,
// so what a unit states about one is a contribution and never a promise.
struct ForeignName {
  std::string linkage_name;
  // A machine function type in the pool the containing record carries.
  mir::TypeId prototype;
  ForeignBody body;
};

// What assembling the program reads about one compiled unit: which namespace
// it brings up and against what, and which foreign names it takes part in.
// These are program-level facts, which is a different question from what the
// unit promises the units that reference it -- a referrer compiles against a
// signature, while nothing here is a name anyone may reach.
//
// It is produced beside the unit's code and outlives it, so the steps that read
// across the design cost a name and a prototype per unit rather than a unit.
// Every fact therefore stands without the unit that produced it: a prototype
// names a type in the pool this record carries, because whoever reads it does
// so where the producing unit's arenas are already gone.
struct UnitProgramRecord {
  std::string unit_name;
  std::optional<NamespaceBringUp> namespace_bring_up;
  mir::TypePool foreign_types;
  std::vector<ForeignName> foreign_names;
  // Whether the unit settles where one of its names lands while the design
  // elaborates rather than while it compiles. The fact is the unit's; what a
  // target can do about it is the backend's, and one that spells a member by
  // writing its name has no form for such a position at all. Stated here
  // because whether a backend can realize the design has to be answerable
  // before any of it is written.
  bool settles_an_elaborated_coordinate = false;
};

// Takes a foreign prototype from one pool into another, answering with the
// reader's own identity for it. The set is the closed one a foreign signature
// can name (LRM 35.5.6, Annex H): a prototype, machine scalars, a borrowed
// pointer to one, and the canonical vector and open-array handles.
auto AdoptForeignType(
    mir::TypePool& into, const mir::TypePool& from, mir::TypeId id)
    -> mir::TypeId;

// Everything assembling the program reads about `unit`, taken out of it.
auto ProgramRecordOf(const mir::CompilationUnit& unit) -> UnitProgramRecord;

}  // namespace lyra::compiler
