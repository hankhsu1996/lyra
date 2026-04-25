#include "lyra/lowering/hir_to_mir/lower_stmt.hpp"

#include <utility>
#include <variant>
#include <vector>

#include "lyra/hir/process.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/support/overloaded.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerStmt(
    const ProcessLoweringState& proc_state, const hir::Process& hir_proc,
    BodyLoweringState& body_state, const hir::Stmt& stmt) -> mir::Stmt {
  auto data = std::visit(
      support::Overloaded{
          [&](const hir::VarDeclStmt& v) -> mir::StmtData {
            return mir::LocalVarDeclStmt{
                .local_var = proc_state.TranslateLocalVar(v.local_var)};
          },
          [&](const hir::ExprStmt& e) -> mir::StmtData {
            return mir::ExprStmt{.expr = body_state.TranslateExpr(e.expr)};
          },
          [&](const hir::BlockStmt& b) -> mir::StmtData {
            std::vector<mir::StmtId> children;
            children.reserve(b.statements.size());
            for (const hir::StmtId child_id : b.statements) {
              const hir::Stmt& child = hir_proc.stmts.at(child_id.value);
              mir::Stmt lowered_child =
                  LowerStmt(proc_state, hir_proc, body_state, child);
              children.push_back(
                  body_state.AppendStmt(std::move(lowered_child)));
            }
            return mir::BlockStmt{.statements = std::move(children)};
          },
      },
      stmt.data);
  return mir::Stmt{
      .label = stmt.label, .data = std::move(data), .child_bodies = {}};
}

}  // namespace lyra::lowering::hir_to_mir
