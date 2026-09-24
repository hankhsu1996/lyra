#pragma once

#include <optional>
#include <span>
#include <string>
#include <vector>

#include "lyra/mir/compilation_unit.hpp"

namespace lyra::dpi {

// One unit's contribution to the C header a user compiles their DPI-C sources
// against, and the file it is written as. The file is reached only through the
// header below, so its name carries nothing; which unit wrote it is in the
// text. A name derived from the unit's would have to answer the target
// language's spelling rules, which this surface deliberately does not have.
struct AbiFragment {
  std::string relpath;
  std::string text;
};

// What `unit` states of the program's foreign name space (LRM 35), as the text
// of its fragment, or nothing where it states none: a prototype for every
// foreign-linkage callable the unit declares -- the imports the C side must
// define, and the exports it may call. It reads the unit alone, so several
// units' fragments may be written at once.
//
// A foreign name is program-global and lives in its own name space rather than
// any unit's (LRM 35.4, 35.7), so several units may each declare one name and
// each one states it. Repeating an identical C prototype is what every C header
// does; where two disagree it is the user's own compiler that says so, which is
// the only party that can, because LRM 35.4 requires C naming and so forbids
// the mangling that would otherwise turn the disagreement into a link error.
//
// What a unit states is target-language-neutral: it projects the same
// prototypes an execution backend links against, so a foreign source compiled
// against it is correct whichever backend runs the design.
auto AbiFragmentOf(const mir::CompilationUnit& unit)
    -> std::optional<std::string>;

// Adds a unit's fragment to those the design has, named by its position among
// them, which is why fragments are added in the order the design lists its
// units: that order is the same from one emission to the next.
void AddAbiFragment(std::vector<AbiFragment>& fragments, std::string text);

// The header a user includes, which is the union of those fragments: it names
// each one and states nothing itself. Assembling it reads a list of fragments
// and no unit, which is what makes the union a step of whatever collects the
// emitted files rather than a step of the compiler. A design that declares no
// DPI-C names none and is included just the same, so no consumer needs a case
// for its absence.
auto RenderAbiHeader(std::span<const AbiFragment> fragments) -> std::string;

}  // namespace lyra::dpi
