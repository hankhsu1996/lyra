#pragma once

#include <vector>

#include "lyra/lir/compilation_unit.hpp"
#include "lyra/support/statistics.hpp"

namespace lyra::lir {

// How many of each kind of thing `unit` holds: one count per pool of the unit,
// the members of its classes and the captures of its closures summed, and one
// more for the functions whose result is a coroutine.
//
// What a function is made of -- its blocks, instructions and values -- is not
// counted: that follows how much the source wrote, which the size of what the
// unit leaves behind already says.
[[nodiscard]] auto CountsOf(const CompilationUnit& unit)
    -> std::vector<support::Count>;

}  // namespace lyra::lir
