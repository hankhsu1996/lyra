#pragma once

namespace lyra::lir {

struct CompilationUnit;

// Checks the LIR place model within a unit and throws InternalError on a
// malformed place: a Load / Store whose base is not projectable to an object,
// whose member projection is out of range, or whose value / result types do not
// line up. A narrow first slice -- it verifies places, not the CFG, call ABI,
// or every instruction; further checks join it as the vocabulary grows. A
// failure is a compiler-bug invariant, not a user diagnostic.
void Verify(const CompilationUnit& unit);

}  // namespace lyra::lir
