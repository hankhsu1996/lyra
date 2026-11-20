#pragma once

namespace lyra::lir {

struct CompilationUnit;

// Checks what a unit's LIR claims about its storage and its control, and throws
// InternalError where a claim does not hold: a load, store or address-of whose
// place and value disagree about what the storage holds, a value put into
// storage that already holds one or where nothing is made, a cast between two
// integral values whose types differ, an element taken by value out of
// anything but a sequence of handles, a union built holding a member its type
// lacks, a tag read of what is no tagged union, a suspension or a completion
// with a departure outside a coroutine body, a block reached only by unwinding
// that can be left another way, and unwinding let out of a function declared
// not to. A failure is a compiler-bug invariant, not a user diagnostic. Every
// unit is checked before a backend reads it.
void Verify(const CompilationUnit& unit);

}  // namespace lyra::lir
