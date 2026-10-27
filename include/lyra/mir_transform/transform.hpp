#pragma once

#include <string>
#include <vector>

#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir_transform/pass.hpp"

namespace lyra::mir_transform {

// The names of the rewrites a build may skip, in the order they run: every one
// that changes what a correct program costs and nothing a later stage reads.
[[nodiscard]] auto OptionalPasses() -> std::vector<std::string>;

// MIR rewritten into MIR, one unit at a time, from the phase HIR-to-MIR builds
// it in to the one every backend reads. Each rewrite keeps what the program
// does and says it only in MIR's own vocabulary, so both backends translate the
// result as they translate anything else and neither knows a rewrite ran. A
// conversion between two phases always runs; an optional rewrite runs unless
// `options` skips it. The unit is checked as it leaves the last rewrite, since
// that is the form every consumer reads -- and as lowered, after each optional
// rewrite and on reaching each phase as well where `options` asks.
auto Transform(mir::CompilationUnit unit, const PassOptions& options)
    -> mir::CompilationUnit;

}  // namespace lyra::mir_transform
