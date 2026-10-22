#pragma once

#include "lyra/hir/type.hpp"
#include "lyra/hir/type_id.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// What a declaration of `type` that writes no initializer starts at (LRM Table
// 6-7): its type's default, except that every event in it names a new
// synchronization object of its own (LRM 6.17) -- the event a declaration
// declares, each element of a fixed-size array of them and each member of a
// structure whose declaration gives that member no initial value (LRM 7.2.2).
// The default is what reading an entry no array has answers (Table 7-1), which
// for an event is `null`, so the objects are given on top of it, one at a
// time. Where they are given is `frame`'s block, ahead of the value read back
// out of the local holding it.
[[nodiscard]] auto BuildDeclaredInitialValue(
    UnitLowerer& unit_lowerer, const WalkFrame& frame, hir::TypeId type)
    -> mir::ExprId;

// Whether a value of `type` holds an event a new position of it is given an
// object for, as above.
[[nodiscard]] auto HoldsADeclaredEvent(
    const hir::TypePool& types, hir::TypeId type) -> bool;

// The same for `count` positions of the array the local `array` holds, from
// the `int` position `first` on, whose elements are of `element`: the elements
// `new[N]` makes start at their type's default value (LRM 7.5.1), which for an
// event is a new object (Table 6-7).
void GiveElementsNewEvents(
    UnitLowerer& unit_lowerer, const WalkFrame& frame, mir::LocalId array,
    mir::TypeId array_type, hir::TypeId element, mir::ExprId first,
    mir::ExprId count);

}  // namespace lyra::lowering::hir_to_mir
