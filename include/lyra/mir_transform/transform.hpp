#pragma once

#include "lyra/mir/compilation_unit.hpp"

namespace lyra::mir_transform {

// MIR rewritten into MIR, one unit at a time, between the lowering that states
// what the source means and every backend reading it. Each rewrite keeps what
// the program does and says it only in MIR's own vocabulary, so both backends
// translate the result as they translate anything else and neither knows a
// rewrite ran. Every build runs the same rewrites, and the unit is verified
// once, as it leaves, since that is the form every consumer reads.
auto Transform(mir::CompilationUnit unit) -> mir::CompilationUnit;

}  // namespace lyra::mir_transform
