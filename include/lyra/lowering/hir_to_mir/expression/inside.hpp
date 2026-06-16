#pragma once

// Lowering of the `inside` operator (LRM 11.4.13). Reduces the item list to
// an OR-chain of per-item membership predicates via `BuildHirInsideItem
// Predicate` from `inside_predicate.hpp`.

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerHirInsideExprProc(
    ProcessLowerer& process, WalkFrame frame, const hir::InsideExpr& in,
    mir::TypeId result_type) -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
