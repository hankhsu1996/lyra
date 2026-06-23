#include "lyra/lowering/hir_to_mir/case_cascade.hpp"

#include <optional>

#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/mir/block_hops.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

auto AppendCaseSnapshot(
    const ModuleLowerer& module, WalkFrame frame, mir::ExprId cond_expr_id)
    -> CaseSnapshotRefs {
  auto& wrapper = *frame.current_block;
  const mir::TypeId sel_type = wrapper.exprs.Get(cond_expr_id).type;

  const mir::LocalId sel_var = wrapper.vars.Add(
      mir::LocalDecl{.name = "_lyra_case_sel", .type = sel_type});
  const mir::ExprId sel_default_init =
      wrapper.exprs.Add(BuildDefaultValueExpr(module, frame, sel_type));
  wrapper.AppendStmt(
      mir::Stmt{
          .label = std::nullopt,
          .data = mir::LocalDeclStmt{
              .target =
                  mir::LocalRef{
                      .hops = mir::BlockHops{.value = 0}, .var = sel_var},
              .init = sel_default_init}});

  const mir::ExprId sel_target_id = wrapper.exprs.Add(
      mir::Expr{
          .data =
              mir::LocalRef{.hops = mir::BlockHops{.value = 0}, .var = sel_var},
          .type = sel_type});
  const mir::ExprId sel_assign_id = wrapper.exprs.Add(
      mir::Expr{
          .data =
              mir::AssignExpr{.target = sel_target_id, .value = cond_expr_id},
          .type = sel_type});
  wrapper.AppendStmt(
      mir::Stmt{
          .label = std::nullopt, .data = mir::ExprStmt{.expr = sel_assign_id}});

  return CaseSnapshotRefs{.sel_var = sel_var, .sel_type = sel_type};
}

}  // namespace lyra::lowering::hir_to_mir
