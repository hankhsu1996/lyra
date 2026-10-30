#pragma once

#include <cstdint>
#include <string_view>
#include <variant>

#include "lyra/lir/function_id.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/support/runtime_object.hpp"

namespace lyra::lir {

struct CompilationUnit;
class Type;

// One step of a value's life that its type says how to carry out: a second
// value equal to one only read, built in storage holding nothing; the value
// built in such storage out of one whose holder still ends it afterwards; one
// value written over the one storage already holds; and the end of a value.
enum class ValueStep : std::uint8_t { kCopy, kMove, kAssign, kEnd };

// The step's stable spelling, which names the function a unit states for it.
auto ValueStepName(ValueStep step) -> std::string_view;

// How a step of a value of one type is carried out, which is the one answer
// every consumer reads, as rustc resolves a type's drop glue once and its code
// generator calls it or, where it is empty, nothing:
//
// - the value's bytes, copied where the step builds or writes and left alone
//   where it ends -- a value with nothing to end, all of whose parts have
//   nothing to end either;
// - the library's own entry for what the value is held as;
// - a function of the unit, built from the type's parts;
// - the definition the value opens with, which a value whose type is known
//   only where the program runs is ended through;
// - no such step, for a value that has none: an owning pointer is never
//   copied, and a closure is ended where it lies and never moved.
struct StepIsBytes {};
struct StepByLibrary {
  support::RuntimeObject object;
};
struct StepByFunction {};
struct StepThroughDefinition {};
struct NoSuchStep {};
using StepRealization = std::variant<
    StepIsBytes, StepByLibrary, StepByFunction, StepThroughDefinition,
    NoSuchStep>;

// Whether a value of `type` is the bytes its type lays out -- a tuple, a
// structure, a union of either kind, a fixed-size unpacked array -- which
// state nothing of it, so whatever holds one is told the type beside it.
[[nodiscard]] auto IsLaidOutByItsType(const Type& type) -> bool;

// Whether ending a value of `type` does anything: it holds something the
// library ends, an object, a closure, or storage it owns. A value laid out by
// its type does exactly where one of its parts does, but an untagged union,
// whose members are only bits another member may have written.
[[nodiscard]] auto NeedsEnd(const CompilationUnit& unit, TypeId type) -> bool;

// How `step` of a value of `type` is carried out in `unit`. Where the answer is
// a function, it is the one the unit states for it, which every value laid out
// by its type with something to end, every closure and every owning pointer to
// a value or a cell has.
[[nodiscard]] auto StepOf(
    const CompilationUnit& unit, TypeId type, ValueStep step)
    -> StepRealization;

// The function `unit` states for `step` of `type`, which a table describing
// the type names. A value laid out by its type has one for each step in every
// unit naming the type, whether or not the step does anything but copy its
// bytes, since the library holding one asks every step of it.
[[nodiscard]] auto StepFunctionOf(
    const CompilationUnit& unit, TypeId type, ValueStep step) -> FunctionId;

}  // namespace lyra::lir
