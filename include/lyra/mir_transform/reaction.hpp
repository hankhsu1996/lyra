#pragma once

#include "lyra/mir/compilation_unit.hpp"

namespace lyra::mir_transform {

// Splits each process body that stops only at the wait closing its loop into
// two plain bodies, which the runtime's one coroutine for every such process
// runs: a setup, which builds the wait into storage it is handed and answers
// whether a pass runs at all, and a pass, which runs the loop's statements once
// and answers whether the wait has still to end. The process's object and the
// wait reach both with their types erased, as the runtime holds them for bodies
// of every class, and each part restores them itself. Every call that started
// such a process now enters the runtime's coroutine on its object and the two
// parts.
//
// A body the rule does not fit stays a coroutine, which is the same program
// at a higher cost to compile.
auto SplitReactions(mir::CompilationUnit unit) -> mir::CompilationUnit;

}  // namespace lyra::mir_transform
