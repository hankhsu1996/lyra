#pragma once

#include "lyra/mir/class_id.hpp"
#include "lyra/mir/code_source.hpp"
#include "lyra/mir/record_id.hpp"
#include "lyra/support/linkage.hpp"

namespace lyra::mir {

struct CompilationUnit;

// How each definition a unit's artifact states is linked, read off what MIR
// states of it, as clang's code generation asks its AST context what linkage a
// declaration gets. No node holds the answer, since how many artifacts define
// a name is no fact of the program. A unit compiled to several bodies is
// several artifacts sharing the unit's names, so what each body states of its
// own and what every body states alike have to be told apart before any of
// them is written, and the backends ask one query for it. A constant of the
// unit is asked nothing here: it is data an artifact holds for its own code
// and its own definitions, under no name another artifact reaches, and a
// target reaching one from a definition every body states alike makes it mean
// the same in each by that target's own rule.

// A class: shared where every body of the unit states it alike, external where
// another unit names it and this artifact defines it, or another unit defines
// it, and internal where nothing outside this artifact names it -- what
// realizes a scope of this body, or what the lowering built for its own use.
// What a class owns -- its constructor, its methods and its definition -- is
// linked as the class is.
[[nodiscard]] auto LinkageOf(const CompilationUnit& unit, ClassId id)
    -> support::Linkage;

// A record: a declaration of the unit another unit names (LRM 7.2, 7.3), which
// no body's code adds to, so every body states it alike; external where another
// unit declares it.
[[nodiscard]] auto LinkageOf(const CompilationUnit& unit, RecordId id)
    -> support::Linkage;

// A foreign name's scope entry (LRM 35.5.3): it is defined from the name and
// the prototype alone, so every unit declaring the name defines it alike.
[[nodiscard]] auto ForeignScopeEntryLinkage() -> support::Linkage;

// The code at `at`: what owns it decides, a class, a record or a foreign name
// as above. A function of the namespace is external where something outside
// this artifact calls it -- by its linkage name, its identifier, or as an entry
// the unit publishes -- and internal where only its position reaches it. A
// closure is built by one site of one body, and an operation of a type with no
// declaration is built for the body that needs it, so both are internal.
[[nodiscard]] auto LinkageOf(const CompilationUnit& unit, const CodeSource& at)
    -> support::Linkage;

}  // namespace lyra::mir
