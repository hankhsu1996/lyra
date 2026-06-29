#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"

#include <utility>

#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

auto SnapshotExprToLocal(
    const ModuleLowerer& module, WalkFrame frame, mir::Block& wrapper,
    std::string name, mir::TypeId type, mir::ExprId expr_id) -> mir::LocalId {
  const mir::LocalId snap_var = frame.bindings->DeclareAnonymous(
      mir::LocalDecl{.name = std::move(name), .type = type});
  const mir::ExprId default_init =
      wrapper.exprs.Add(BuildDefaultValueExpr(module, frame, type));
  wrapper.AppendStmt(
      mir::LocalDeclStmt{.target = snap_var, .init = default_init});

  const mir::ExprId target_id =
      wrapper.exprs.Add(mir::MakeLocalRefExpr(snap_var, type));
  const mir::ExprId assign_id = wrapper.exprs.Add(
      mir::Expr{
          .data = mir::AssignExpr{.target = target_id, .value = expr_id},
          .type = type});
  wrapper.AppendStmt(mir::ExprStmt{.expr = assign_id});

  return snap_var;
}

}  // namespace lyra::lowering::hir_to_mir
