#include "lyra/lowering/hir_to_mir/case_cascade.hpp"

#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

auto AppendCaseSnapshot(
    const UnitLowerer& unit_lowerer, WalkFrame frame, mir::ExprId cond_expr_id)
    -> CaseSnapshotRefs {
  auto& wrapper = *frame.current_block;
  const mir::TypeId sel_type = wrapper.exprs.Get(cond_expr_id).type;
  const mir::LocalId sel_var = SnapshotExprToLocal(
      unit_lowerer, frame, wrapper, "_lyra_case_sel", sel_type, cond_expr_id);
  return CaseSnapshotRefs{.sel_var = sel_var, .sel_type = sel_type};
}

}  // namespace lyra::lowering::hir_to_mir
