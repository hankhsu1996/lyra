#include "lyra/lowering/hir_to_mir/lower_process.hpp"

#include <cstddef>
#include <cstdint>
#include <utility>

#include "lyra/hir/expr.hpp"
#include "lyra/hir/local_var.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/hir_to_mir/lower_expr.hpp"
#include "lyra/lowering/hir_to_mir/lower_stmt.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/local_var.hpp"
#include "lyra/mir/process.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/support/internal_error.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto LowerProcessKind(hir::ProcessKind kind) -> mir::ProcessKind {
  switch (kind) {
    case hir::ProcessKind::kInitial:
      return mir::ProcessKind::kInitial;
  }
  throw support::InternalError("LowerProcessKind: unknown HIR ProcessKind");
}

}  // namespace

auto LowerProcess(
    const UnitLoweringState& unit_state, const ClassLoweringState& class_state,
    const hir::StructuralScope& process_scope, const hir::Process& src)
    -> mir::Process {
  ScopeStack stack;
  const ScopeStackGuard guard(stack, process_scope);

  ProcessLoweringState proc_state;
  for (std::size_t i = 0; i < src.local_vars.size(); ++i) {
    const auto& hir_local = src.local_vars[i];
    const auto mir_id = proc_state.AddLocalVar(
        mir::LocalVar{
            .name = hir_local.name,
            .type = unit_state.TranslateType(hir_local.type)});
    proc_state.MapLocalVar(
        hir::LocalVarId{static_cast<std::uint32_t>(i)}, mir_id);
  }

  BodyLoweringState body_state;
  for (std::size_t i = 0; i < src.exprs.size(); ++i) {
    const hir::ExprId hir_id{static_cast<std::uint32_t>(i)};
    body_state.AppendExpr(
        hir_id, mir::Expr{
                    .data = LowerProcessExprData(
                        unit_state, class_state, proc_state, body_state, stack,
                        src, src.exprs[i].data)});
  }

  const hir::Stmt& root = src.stmts.at(src.body.value);
  mir::Stmt lowered_root = LowerStmt(proc_state, src, body_state, root);
  body_state.AppendRootStmt(body_state.AppendStmt(std::move(lowered_root)));

  auto body = body_state.Finish();
  body.local_vars = proc_state.MoveLocals();
  return mir::Process{
      .kind = LowerProcessKind(src.kind), .body = std::move(body)};
}

}  // namespace lyra::lowering::hir_to_mir
