#pragma once

#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr_id.hpp"

namespace lyra::mir {
struct Block;
}  // namespace lyra::mir

namespace lyra::lowering::hir_to_mir {

// Reduces a lowered expression consumed as a condition to a stated boolean
// predicate, appending a `BoolCastExpr` over it to `block` and returning its
// id. Every condition context (if / while / for / do-while / ternary) stores
// the reduced predicate, so a backend emits the reduction mechanically from the
// node and never re-derives it from the operand's value type (LRM 12.4: the
// condition is true when the expression is nonzero, and false when it is zero,
// x, or z).
[[nodiscard]] auto ReduceToCondition(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId cond)
    -> mir::ExprId;

}  // namespace lyra::lowering::hir_to_mir
