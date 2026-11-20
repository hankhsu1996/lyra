#pragma once

#include <variant>

#include "lyra/lir/function_id.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/support/runtime_object.hpp"
#include "lyra/support/value_step.hpp"

namespace lyra::lir {

struct CompilationUnit;
class Type;

// How a step of a value of one type is carried out, which is the one answer
// every consumer reads, as clang's `DestructionKind` and `PrimitiveCopyKind`
// answer it for a C type and rustc resolves a type's drop glue once:
//
// - trivial: the value's bytes, copied where the step builds, relocates or
//   writes, and left alone where it ends -- a value with nothing to end, all
//   of whose parts have nothing to end either, and the relocation of a value
//   each of whose parts relocates as its bytes, since a relocation ends
//   nothing of its source;
// - the library's own entry for what the value is held as, as a class's own
//   destructor is;
// - glue: a function of the unit, built from the type's parts, as rustc's
//   drop glue and clone shims are;
// - virtual: the definition the value opens with, which a value whose type is
//   known only where the program runs is ended through, as a virtual
//   destructor is;
// - box: what an owning pointer points at, ended as its type says where the
//   pointer ends, and the storage then given back, as rustc's drop
//   elaboration writes a `Box`'s drop out where it is dropped rather than
//   calling a function per pointee;
// - deleted: no such step, for a value that has none: an owning pointer is
//   never copied, and a closure is ended where it lies and never relocated.
struct TrivialStep {};
struct LibraryStep {
  support::RuntimeObject object;
};
struct GlueStep {};
struct VirtualStep {};
struct BoxStep {};
struct DeletedStep {};
using StepKind = std::variant<
    TrivialStep, LibraryStep, GlueStep, VirtualStep, BoxStep, DeletedStep>;

// Whether a value of `type` is a scalar the IR lays out -- an integral value, a
// real, a reference owning nothing -- held as its own bytes, which every
// holder copies, moves and ends as those bytes.
[[nodiscard]] auto IsScalar(const Type& type) -> bool;

// Whether a value of `type` is the bytes its type lays out -- a tuple, a
// structure, a union of either kind, a fixed-size unpacked array -- which
// state nothing of it, so whatever holds one is told the type beside it.
[[nodiscard]] auto IsLaidOutByItsType(const Type& type) -> bool;

// Whether ending a value of `type` does anything: it holds something the
// library ends, an object, a closure, or storage it owns. A value laid out by
// its type does exactly where one of its parts does.
[[nodiscard]] auto NeedsEnd(const CompilationUnit& unit, TypeId type) -> bool;

// How `step` of a value of `type` is carried out in `unit`. Where the answer is
// a function, it is the one the unit states for it, which every value laid out
// by its type with something to end and every closure has.
[[nodiscard]] auto StepOf(
    const CompilationUnit& unit, TypeId type, support::ValueStep step)
    -> StepKind;

// The function `unit` states for `step` of `type`, which a table describing
// the type names. A value laid out by its type has one for each step in every
// unit naming the type, whether or not the step does anything but copy its
// bytes, since the library holding one asks every step of it.
[[nodiscard]] auto StepFunctionOf(
    const CompilationUnit& unit, TypeId type, support::ValueStep step)
    -> FunctionId;

}  // namespace lyra::lir
