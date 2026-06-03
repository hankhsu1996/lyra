#pragma once

#include <vector>

#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// Synthesizes a primitive MIR expression that evaluates to the LRM Table 6-7
// default value of `type_id`, appends it to `scope_state`, and returns its id.
//
// `int x;` and `int x = 0;` are different in SV source -- HIR preserves that
// distinction via `optional<initializer>`. By MIR every variable has an
// explicit initializer expression: the SV "no initializer means LRM default"
// sugar is decomposed into a primitive Expr at the HIR-to-MIR boundary so
// downstream layers see one shape (an Expr) instead of two.
[[nodiscard]] auto SynthesizeDefaultValueExpr(
    const UnitLoweringState& unit_state,
    ProceduralScopeLoweringState& scope_state, mir::TypeId type_id)
    -> mir::ExprId;

// Wraps a list of element ExprIds destined for an `UnpackedArrayType`
// constructor in `ConstructExpr{[element_default,
// ArrayLiteralExpr{elements}]}`. This is the construction shape every site that
// produces an unpacked-array value must use: the `default_value_` member
// required by `UnpackedArray<T>`'s runtime ctor is supplied here via
// `SynthesizeDefaultValueExpr` on the element type, and the elements ride in an
// `ArrayLiteralExpr` that the `ConstructExpr` renderer emits as a
// brace-init-list. See `docs/decisions/runtime-shape-and-default-value.md`.
[[nodiscard]] auto BuildUnpackedArrayConstructExpr(
    const UnitLoweringState& unit_state,
    ProceduralScopeLoweringState& scope_state, mir::TypeId unpacked_type_id,
    std::vector<mir::ExprId> elements) -> mir::Expr;

}  // namespace lyra::lowering::hir_to_mir
