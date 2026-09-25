#pragma once

namespace lyra::lir {

struct CompilationUnit;

// Checks what a unit's LIR claims about its storage and its control, and throws
// InternalError where a claim does not hold: a load, store or address-of whose
// place and value disagree about what the storage holds, a cast that changes a
// packed value's representation, a part that is storage of its own reached by
// value projection, a suspension outside a coroutine body, and a landing no
// departing call names. A failure is a compiler-bug invariant, not a user
// diagnostic. Every unit is checked before a backend reads it.
void Verify(const CompilationUnit& unit);

}  // namespace lyra::lir
