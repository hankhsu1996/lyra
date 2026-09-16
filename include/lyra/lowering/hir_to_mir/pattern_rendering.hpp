#pragma once

// LRM 21.2.1.6 assignment-pattern format. A structure prints its members under
// the names its type declares for them, a union prints only its first declared
// member, a tagged union prints the member its tag names, and an enumeration
// prints the name declared for the value. None of that is carried by a value,
// and a formatter reaching an element of a container has no way back to the
// type the element came from -- so the rendering is composed here, where the
// SystemVerilog type is still in hand, as a callable the unit owns once per
// type. What the print operation carries is the text that callable answers
// with.

#include "lyra/hir/compilation_unit.hpp"
#include "lyra/hir/type_id.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/expr_id.hpp"

namespace lyra::lowering::hir_to_mir {

// What answers for how a value of one type reads under LRM 21.2.1.6. A
// declaration is all this is a function of, so it is the same answer wherever
// the value came from and whatever the site asking happens to be doing.
enum class PatternReading : std::uint8_t {
  // The text follows from the value and the conversion alone -- an integral, a
  // real, a string, a handle -- which the runtime formatter answers.
  kTheValueAnswers,
  // An enumeration declares a name for the value, and the value still reads as
  // its base integral under a radix (LRM 21.2.1.2), so a site that has not yet
  // met its conversion carries both.
  kTheTypeNamesTheValue,
  // An aggregate or a container is its assignment pattern, and the clause
  // defines no other conversion for it, so the text is the whole of what reads.
  kTheTypeNamesItsMembers,
  // The clause asks for the entries of a container the standard gives no way to
  // enumerate. LRM 7.9.4 through 7.9.7 each refuse `first`, `last`, `next` and
  // `prev` to an associative array with a wildcard index, and LRM 7.8.1 refuses
  // it a `foreach` -- the index has no width until a value is stored under it,
  // so there is no variable an entry's index could be read into.
  kNothingCanAnswer,
};

[[nodiscard]] auto PatternReadingOf(
    const hir::CompilationUnit& hir, hir::TypeId type) -> PatternReading;

// Whether the unit owns a callable answering this reading, which is what makes
// the type's text a call rather than the value's own formatting.
[[nodiscard]] auto TypeOwnsItsText(PatternReading reading) -> bool;

// The body of that callable: one value parameter, no object, answering the
// `string` the type reads as. Built where the unit's declarations are settled,
// so it reads nothing of any site that goes on to call it.
[[nodiscard]] auto BuildAssignmentPatternTextCode(
    UnitLowerer& unit_lowerer, hir::TypeId type) -> mir::CallableCode;

// The `string` text LRM 21.2.1.6 reads `value` as, as a call to the callable
// its type owns. `value` is an expression of the block `frame` names, and so is
// the answer. Only for a type whose text is not the value's own.
[[nodiscard]] auto BuildAssignmentPatternText(
    UnitLowerer& unit_lowerer, WalkFrame frame, mir::ExprId value,
    hir::TypeId type) -> mir::ExprId;

}  // namespace lyra::lowering::hir_to_mir
