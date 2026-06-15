#pragma once

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/inside_item.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// Builds the 1-bit MIR predicate for one `inside`-style item against a
// pre-lowered LHS:
//   - value items lower to asymmetric wildcard equality (LRM 11.4.6),
//   - range items lower to `(lhs >= lo) && (lhs <= hi)`.
// Shared between the inside operator (LRM 11.4.13) and the case-inside
// cascade (LRM 12.5.4). Callers OR-chain the per-item results to obtain the
// final inside predicate.
auto BuildHirInsideItemPredicate(
    ProcessLowerer& proc, WalkFrame frame, mir::ExprId lhs_id,
    const hir::InsideItem& item, mir::TypeId result_type)
    -> diag::Result<mir::ExprId>;

}  // namespace lyra::lowering::hir_to_mir
