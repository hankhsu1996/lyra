#include "lyra/lowering/hir_to_mir/case_cascade.hpp"

#include <optional>

#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/procedural_hops.hpp"
#include "lyra/mir/procedural_var.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

auto AppendCaseSnapshot(
    ProceduralScopeLoweringState& wrapper_state, mir::ExprId cond_expr_id)
    -> CaseSnapshotRefs {
  const mir::TypeId sel_type = wrapper_state.GetExpr(cond_expr_id).type;

  const mir::ProceduralVarId sel_var = wrapper_state.AddProceduralVar(
      mir::ProceduralVarDecl{.name = "_lyra_case_sel", .type = sel_type});
  const mir::StmtId sel_decl_id = wrapper_state.AddStmt(
      mir::Stmt{
          .label = std::nullopt,
          .data =
              mir::ProceduralVarDeclStmt{
                  .target =
                      mir::ProceduralVarRef{
                          .hops = mir::ProceduralHops{.value = 0},
                          .var = sel_var},
                  .init = std::nullopt},
          .child_procedural_scopes = {}});
  wrapper_state.AddRootStmt(sel_decl_id);

  const mir::ExprId sel_assign_id = wrapper_state.AddExpr(
      mir::Expr{
          .data =
              mir::AssignExpr{
                  .target =
                      mir::Lvalue{
                          .root =
                              mir::ProceduralVarRef{
                                  .hops = mir::ProceduralHops{.value = 0},
                                  .var = sel_var},
                          .selectors = {}},
                  .value = cond_expr_id},
          .type = sel_type});
  const mir::StmtId sel_assign_stmt_id = wrapper_state.AddStmt(
      mir::Stmt{
          .label = std::nullopt,
          .data = mir::ExprStmt{.expr = sel_assign_id},
          .child_procedural_scopes = {}});
  wrapper_state.AddRootStmt(sel_assign_stmt_id);

  return CaseSnapshotRefs{.sel_var = sel_var, .sel_type = sel_type};
}

}  // namespace lyra::lowering::hir_to_mir
