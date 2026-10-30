#pragma once

#include <string>

#include "lyra/hir/type_id.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

class UnitLowerer;

// The text every type equivalent to `type` shares and no other type does (LRM
// 6.22.2), which a value of `dynamic_type` is kept with and tested by when it
// is received (LRM 15.4.5). Equivalence follows a type's own structure where
// the standard says it does -- an integral type is its width, its states and
// its signedness, an array its kind, its element and its count -- and its
// declaration where the standard identifies a type by one: an enumeration, a
// structure, a union, a class. A declaration is named by its unit and its path
// in that unit, so every unit computes the same text for one type alone, as
// rustc computes a type's `TypeId` in any crate.
[[nodiscard]] auto EquivalenceIdentityOf(
    const UnitLowerer& unit_lowerer, hir::TypeId type) -> std::string;

// `type` with the bound dropped from every queue it is made of: a bound is no
// part of which type a queue is (LRM 6.22.2 e), so every type equivalent to
// `type` has this one form. A value of any type is kept at it, and a value is
// assigned across two such types by passing through it; a bound is applied
// where a value is received into a variable declared with one, as any
// assignment to it applies it (LRM 7.10.5). A structure or a union is
// equivalent only to itself, so it is kept as it was declared.
[[nodiscard]] auto WithoutQueueBounds(
    const mir::TypePool& types, mir::TypeId type) -> mir::TypeId;

// Whether `type` is an array holding a bounded queue somewhere below its
// outermost dimension, so that a value of it and one of its form with no
// queue bounded differ element by element rather than only in what the
// outermost dimension admits.
[[nodiscard]] auto HoldsBoundedQueueBelow(
    const mir::TypePool& types, mir::TypeId type) -> bool;

}  // namespace lyra::lowering::hir_to_mir
