#pragma once

#include <vector>

#include "lyra/lowering/hir_to_mir/module_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// Builds a primitive MIR expression evaluating to the LRM Table 6-7 default
// value of `type`, returning the top node detached for the caller to intern.
// Composite types (UnpackedArray, DynamicArray, Queue) recurse and register
// each element default into `frame.current_procedural_scope` as children before
// returning the outer composite, so interning the result yields a
// self-contained subtree of arena entries.
//
// `int x;` and `int x = 0;` are different in SV source -- HIR preserves that
// distinction via `optional<initializer>`. By MIR every variable has an
// explicit initializer expression: the SV "no initializer means LRM default"
// sugar is decomposed into a primitive Expr at the HIR-to-MIR boundary so
// downstream layers see one shape (an Expr) instead of two.
[[nodiscard]] auto BuildDefaultValueExpr(
    const ModuleLowerer& module, WalkFrame frame, mir::TypeId type)
    -> mir::Expr;

// Wraps a list of element ExprIds destined for an array container constructor
// (`UnpackedArrayType` or `DynamicArrayType`) in a construction call whose
// arguments are `[element_default, ArrayLiteralExpr{elements}]`. This is the
// construction shape every site that produces an array-container value must
// use: the canonical-default element required by the wrapper's runtime ctor
// (to seed `oob_slot_`) is supplied here via `BuildDefaultValueExpr` on
// the element type, and the elements ride in an `ArrayLiteralExpr` that the
// renderer emits as `std::array<T, N>{...}`. See
// `docs/decisions/runtime-shape-and-default-value.md`.
[[nodiscard]] auto BuildArrayConstructionCall(
    const ModuleLowerer& module, WalkFrame frame, mir::TypeId array_type,
    std::vector<mir::ExprId> elements) -> mir::Expr;

}  // namespace lyra::lowering::hir_to_mir
