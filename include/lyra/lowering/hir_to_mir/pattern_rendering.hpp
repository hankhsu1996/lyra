#pragma once

// LRM 21.2.1.6 assignment-pattern format. A structure prints its members under
// the names its type declares for them, a union prints only its first declared
// member, a tagged union prints the member its tag names, and an enumeration
// prints the name declared for the value. None of that is carried by a value,
// and a formatter reaching an element of a container has no way back to the
// type the element came from -- so the rendering is composed here, where the
// SystemVerilog type is still in hand, as a callable synthesized once per type.
// What the print operation carries is the text that callable answers with.

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/type_id.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr_id.hpp"

namespace lyra::lowering::hir_to_mir {

// What a type states about how a value of it reads (LRM 21.2.1.6), which is
// what a caller assembling a print operand has to know.
enum class PatternRendering : std::uint8_t {
  // The text follows from the value and the conversion alone -- an integral, a
  // real, a string, a handle -- which the runtime formatter answers as it
  // always has.
  kValueDecides,
  // The type declares a text, and the value still reads as itself under every
  // other conversion: an enumeration prints its declared name under `%p` and
  // its base integral under a radix.
  kBesideTheValue,
  // The type declares the only text the clause defines for it, so nothing else
  // is left to read: an aggregate is its assignment pattern.
  kInsteadOfTheValue,
};

[[nodiscard]] auto PatternRenderingOf(
    const UnitLowerer& unit_lowerer, hir::TypeId type) -> PatternRendering;

// Whether the type states a text at all, for a caller that has no use for
// which of the two ways it states one.
[[nodiscard]] auto TypeStatesItsRendering(PatternRendering rendering) -> bool;

// The `string` text LRM 21.2.1.6 renders `value` as, as a call to the
// rendering synthesized for its type -- synthesized on first use and shared by
// every site in the unit. `value` is an expression of the block `frame` names,
// and so is the answer. Only for a type whose rendering is not the value's own.
[[nodiscard]] auto BuildPatternRendering(
    UnitLowerer& unit_lowerer, WalkFrame frame, mir::ExprId value,
    hir::TypeId type, diag::SourceSpan span) -> diag::Result<mir::ExprId>;

}  // namespace lyra::lowering::hir_to_mir
