#include "lyra/lowering/hir_to_mir/statement/blocks.hpp"

#include <expected>
#include <optional>
#include <string>
#include <utility>

#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerEmptyStmt(std::optional<std::string> label)
    -> diag::Result<mir::Stmt> {
  return mir::Stmt{.label = std::move(label), .data = mir::EmptyStmt{}};
}

auto LowerBlockStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::BlockStmt& b) -> diag::Result<mir::Stmt> {
  const hir::ProceduralBody& hir_proc = process.HirBody();
  mir::ProceduralScope child_proc_scope;
  const WalkFrame child_frame =
      frame.WithProceduralScope(&child_proc_scope).Deeper();
  for (const hir::StmtId child_hir_id : b.statements) {
    const hir::Stmt& child = hir_proc.stmts.at(child_hir_id.value);
    auto lowered = process.LowerStmt(child, child_frame);
    if (!lowered) {
      return std::unexpected(std::move(lowered.error()));
    }
    child_proc_scope.AppendStmt(*std::move(lowered));
  }
  const mir::ProceduralScopeId scope_id =
      frame.current_procedural_scope->AddChildScope(
          std::move(child_proc_scope));
  return mir::Stmt{
      .label = std::move(label), .data = mir::BlockStmt{.scope = scope_id}};
}

auto LowerStmtIntoChildScope(
    ProcessLowerer& process, WalkFrame frame, hir::StmtId hir_stmt_id)
    -> diag::Result<mir::ProceduralScope> {
  mir::ProceduralScope child_scope;
  const WalkFrame child_frame =
      frame.WithProceduralScope(&child_scope).Deeper();
  const hir::Stmt& hir_stmt = process.HirBody().stmts.at(hir_stmt_id.value);
  auto lowered = process.LowerStmt(hir_stmt, child_frame);
  if (!lowered) {
    return std::unexpected(std::move(lowered.error()));
  }
  child_scope.AppendStmt(*std::move(lowered));
  return std::move(child_scope);
}

}  // namespace lyra::lowering::hir_to_mir
