#include "lyra/lowering/hir_to_mir/lower_process.hpp"

#include <cstddef>
#include <cstdint>
#include <utility>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/local_var.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/lowering/hir_to_mir/lower_expr.hpp"
#include "lyra/lowering/hir_to_mir/lower_stmt.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/local_var.hpp"
#include "lyra/mir/process.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto LowerProcessKind(hir::ProcessKind kind) -> mir::ProcessKind {
  switch (kind) {
    case hir::ProcessKind::kInitial:
      return mir::ProcessKind::kInitial;
  }
  throw InternalError("LowerProcessKind: unknown HIR ProcessKind");
}

}  // namespace

auto LowerProcess(
    const UnitLoweringState& unit_state, const ClassLoweringState& class_state,
    const hir::Process& src) -> diag::Result<mir::Process> {
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
    const auto& hir_expr = src.exprs[i];
    auto expr_or = LowerProcessExpr(
        unit_state, class_state, proc_state, body_state, src, hir_expr);
    if (!expr_or) {
      return std::unexpected(std::move(expr_or.error()));
    }
    body_state.AppendExpr(hir_id, *std::move(expr_or));
  }

  const hir::Stmt& root = src.stmts.at(src.body.value);
  auto lowered_root_or =
      LowerStmt(unit_state, proc_state, src, body_state, root);
  if (!lowered_root_or) {
    return std::unexpected(std::move(lowered_root_or.error()));
  }
  body_state.AppendRootStmt(body_state.AppendStmt(*std::move(lowered_root_or)));

  auto body = body_state.Finish();
  body.local_vars = proc_state.MoveLocals();
  return mir::Process{
      .kind = LowerProcessKind(src.kind), .body = std::move(body)};
}

}  // namespace lyra::lowering::hir_to_mir
