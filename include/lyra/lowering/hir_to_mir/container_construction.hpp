#pragma once

#include <optional>
#include <utility>
#include <vector>

#include "lyra/mir/builder.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// A container's element type, for a caller whose own construction guarantees
// there is one. Which types hold a sequence of elements is the type's own
// question and is answered there; this adds only what an absent answer means
// here. Where the guarantee did not hold, the producer built something it
// should not have, so this reports a compiler bug rather than answering.
[[nodiscard]] auto RequiredContainerElementType(
    const mir::CompilationUnit& unit, mir::TypeId container) -> mir::TypeId;

// Builds a fixed-size unpacked array from `repeat_unit`, laid down again and
// again until the array is full: every element of the array stated once (LRM
// 10.9), one element default (LRM Table 7-1), or the items of a `'{n{...}}`
// pattern (LRM 10.9.1). The array's type fixes how many elements it has, so
// the construction's one argument is the unit, and the value's MIR stays
// O(repeat_unit) however large the array is. The unit holds at least one
// element and a whole number of them fills the array.
[[nodiscard]] auto BuildFixedArrayConstructionCall(
    const mir::CompilationUnit& unit, mir::Builder block,
    mir::TypeId array_type, std::vector<mir::ExprId> repeat_unit) -> mir::Expr;

// Builds an array-shaped container from an explicit element list, laid down
// once. A fixed-size unpacked array is the construction above over the whole
// list. A queue and a dynamic array have no size of their own, so their
// arguments are `[elements, 1]`.
[[nodiscard]] auto BuildArrayConstructionCall(
    const mir::CompilationUnit& unit, mir::Builder block,
    mir::TypeId array_type, std::vector<mir::ExprId> elements) -> mir::Expr;

// Builds a queue or a dynamic array holding `count` replications of
// `repeat_unit` (LRM 10.9.1), in the argument shape of the construction call
// above with the count where it has one, so the value's MIR stays
// O(repeat_unit) rather than O(repeat_unit * count). A fixed-size unpacked
// array states no count and is built by its own construction.
[[nodiscard]] auto BuildArrayRepeatCall(
    const mir::CompilationUnit& unit, mir::Builder block,
    mir::TypeId array_type, std::vector<mir::ExprId> repeat_unit,
    mir::ExprId count_id) -> mir::Expr;

// Builds the construction call for a sequence: the values it holds, in order.
// A sequence is built from its values alone and repeats nothing, so the element
// list is the constructor's one argument, and a sequence holding nothing is
// that list with no elements rather than a second form of the call.
[[nodiscard]] auto BuildSequenceConstructionCall(
    const mir::CompilationUnit& unit, mir::Builder block,
    mir::TypeId sequence_type, std::vector<mir::ExprId> elements) -> mir::Expr;

// Builds the value of an associative-array literal (LRM 7.9.11). Each (key,
// value) entry is a pair, and the entries ride in the plain-data array of
// those pairs, which is the construction's one argument. Where the literal
// writes a `default:` clause, what a read of an absent key returns (LRM 7.8.6)
// is a value of that array which only the literal can state, so the built
// array is then given it by the operation that says so. Where it writes none,
// such a read returns what the element type says and nothing is stated for it.
[[nodiscard]] auto BuildAssociativeConstructionCall(
    const mir::CompilationUnit& unit, mir::Builder block,
    mir::TypeId assoc_type,
    std::vector<std::pair<mir::ExprId, mir::ExprId>> entries,
    std::optional<mir::ExprId> stated_default) -> mir::Expr;

}  // namespace lyra::lowering::hir_to_mir
