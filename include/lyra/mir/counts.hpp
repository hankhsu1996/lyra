#pragma once

#include <cstdint>
#include <vector>

#include "lyra/mir/compilation_unit.hpp"
#include "lyra/support/statistics.hpp"

namespace lyra::mir {

// How many of each kind of thing `unit` holds: one count per pool of the unit,
// a class's pools summed over its classes, and one more for the bodies whose
// result is a coroutine, which is what a body that waits costs beyond one that
// does not.
//
// What a body is made of -- its statements, expressions and locals -- is not
// counted: that follows how much the source wrote, which the size of what the
// unit leaves behind already says.
[[nodiscard]] auto CountsOf(const CompilationUnit& unit)
    -> std::vector<support::Count>;

// How deep `unit`'s bodies nest, at the deepest, their blocks and the
// expressions in them alike. Every consumer walks a body by its nesting and a
// target compiler bounds how deeply text may nest, so a depth that follows how
// much the source wrote rather than how deeply it nested is what breaks a
// build.
[[nodiscard]] auto NestingDepthOf(const CompilationUnit& unit) -> std::uint64_t;

}  // namespace lyra::mir
