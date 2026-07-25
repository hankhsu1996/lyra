#pragma once

#include <span>
#include <string>

#include "lyra/mir/compilation_unit.hpp"

namespace lyra::dpi {

// Renders the C header a user compiles their DPI-C sources against (LRM 35):
// one prototype per foreign-linkage callable the design declares -- the imports
// the C side must define, and the exports it may call. A foreign name is
// program-global and lives in its own name space rather than any unit's (LRM
// 35.4, 35.7), so the surface is collected across the whole design and the
// several units that may each declare one name collapse to its one prototype. A
// design with no DPI-C declaration renders the same header with no prototypes.
//
// The result is target-language-neutral: it projects the same prototypes an
// execution backend links against, so a foreign source compiled against it is
// correct whichever backend runs the design.
auto RenderAbiHeader(
    std::span<const mir::CompilationUnit> units,
    const mir::CompilationUnit& root) -> std::string;

}  // namespace lyra::dpi
