#include "lyra/lowering/hir_to_mir/lower_stmt.hpp"

#include <expected>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/lower_expr.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerStmt(
    const UnitLoweringState& unit_state, const ClassLoweringState& class_state,
    const ProcessLoweringState& proc_state, BodyLoweringState& body_state,
    const hir::Process& hir_proc, const hir::Stmt& stmt)
    -> diag::Result<mir::Stmt> {
  auto data = std::visit(
      Overloaded{
          [&](const hir::VarDeclStmt& v) -> diag::Result<mir::StmtData> {
            return mir::LocalVarDeclStmt{
                .target = proc_state.TranslateLocalVar(v.local_var)};
          },
          [&](const hir::ExprStmt& e) -> diag::Result<mir::StmtData> {
            auto expr_or = LowerExpr(
                unit_state, class_state, proc_state, body_state, hir_proc,
                hir_proc.exprs.at(e.expr.value));
            if (!expr_or) {
              return std::unexpected(std::move(expr_or.error()));
            }
            return mir::ExprStmt{
                .expr = body_state.AddExpr(*std::move(expr_or))};
          },
          [&](const hir::BlockStmt& b) -> diag::Result<mir::StmtData> {
            std::vector<mir::StmtId> children;
            children.reserve(b.statements.size());
            for (const hir::StmtId child_id : b.statements) {
              const hir::Stmt& child = hir_proc.stmts.at(child_id.value);
              auto lowered_child = LowerStmt(
                  unit_state, class_state, proc_state, body_state, hir_proc,
                  child);
              if (!lowered_child) {
                return std::unexpected(std::move(lowered_child.error()));
              }
              children.push_back(body_state.AddStmt(*std::move(lowered_child)));
            }
            return mir::BlockStmt{.statements = std::move(children)};
          },
      },
      stmt.data);
  if (!data) return std::unexpected(std::move(data.error()));
  return mir::Stmt{
      .label = stmt.label, .data = *std::move(data), .child_bodies = {}};
}

}  // namespace lyra::lowering::hir_to_mir
