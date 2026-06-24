#pragma once

// Lowering of the `inside` operator (LRM 11.4.13). Reduces the item list to
// an OR-chain of per-item membership predicates via `BuildHirInsideItem
// Predicate` from `inside_predicate.hpp`.

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/hir_to_mir/expression/expr_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// The `inside` operator's meaning is independent of the enclosing scope, so one
// template over the pass class serves both contexts; explicit instantiations
// live in the implementation file.
template <ExprLowerer Lowerer>
auto LowerHirInsideExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::InsideExpr& in,
    mir::TypeId result_type) -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
