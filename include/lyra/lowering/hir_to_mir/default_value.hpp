#pragma once

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

}  // namespace lyra::lowering::hir_to_mir
