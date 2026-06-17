#include "lyra/lowering/hir_to_mir/case_cascade.hpp"

#include <optional>

#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/procedural_hops.hpp"
#include "lyra/mir/procedural_var.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

auto AppendCaseSnapshot(
    const ModuleLowerer& module, WalkFrame frame, mir::ExprId cond_expr_id)
    -> CaseSnapshotRefs {
  auto& wrapper = *frame.current_procedural_scope;
  const mir::TypeId sel_type = wrapper.GetExpr(cond_expr_id).type;

  const mir::ProceduralVarId sel_var = wrapper.AddProceduralVar(
      mir::ProceduralVarDecl{.name = "_lyra_case_sel", .type = sel_type});
  const mir::ExprId sel_default_init =
      AddDefaultValueExpr(module, frame, sel_type);
  wrapper.AppendStmt(
      mir::Stmt{
          .label = std::nullopt,
          .data = mir::ProceduralVarDeclStmt{
              .target =
                  mir::ProceduralVarRef{
                      .hops = mir::ProceduralHops{.value = 0}, .var = sel_var},
              .init = sel_default_init}});

  const mir::ExprId sel_target_id = wrapper.AddExpr(
      mir::Expr{
          .data =
              mir::ProceduralVarRef{
                  .hops = mir::ProceduralHops{.value = 0}, .var = sel_var},
          .type = sel_type});
  const mir::ExprId sel_assign_id = wrapper.AddExpr(
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
