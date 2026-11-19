#pragma once

#include "lyra/mir/compilation_unit.hpp"

namespace lyra::mir_transform {

// Splits each process body that stops only at the wait closing its loop into
// two plain bodies the run calls: a setup, which builds the wait into storage
// it is handed and answers whether a pass runs at all, and a pass, which runs
// the loop's statements once and answers whether the wait has still to end.
// The process's object and the wait reach both with their types erased, as the
// run holds them for bodies of every class, and each part restores them
// itself. A body that never stops becomes its own setup, which answers that no
// pass follows. Every registration of such a process hands it to the run as a
// reaction: its object and a constant of the unit naming its parts.
//
// Any other body the rule does not fit stays a coroutine, which is the same
// program at a higher cost to compile and to run. A class another unit can
// name keeps its bodies as they are.
auto SplitReactions(mir::CompilationUnit unit) -> mir::CompilationUnit;

}  // namespace lyra::mir_transform
