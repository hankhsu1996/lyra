#pragma once

#include <vector>

#include "lyra/hir/expr.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/stmt.hpp"

namespace lyra::hir {

// The statement tree of a process or subroutine together with the arenas its
// statements, expressions, and local variables index into. Processes and
// subroutines are both procedural bodies; they differ only in what wraps the
// body (a process kind and sensitivity, or a subroutine signature).
struct ProceduralBody {
  StmtId root_stmt{};
  std::vector<Expr> exprs;
  std::vector<Stmt> stmts;
  std::vector<ProceduralVarDecl> procedural_vars;

  [[nodiscard]] auto GetExpr(ExprId id) const -> const Expr& {
    return exprs.at(id.value);
  }
  [[nodiscard]] auto GetStmt(StmtId id) const -> const Stmt& {
    return stmts.at(id.value);
  }
  [[nodiscard]] auto GetProceduralVar(ProceduralVarId id) const
      -> const ProceduralVarDecl& {
    return procedural_vars.at(id.value);
  }

  auto AddExpr(Expr expr) -> ExprId {
    const ExprId id{static_cast<std::uint32_t>(exprs.size())};
    exprs.push_back(std::move(expr));
    return id;
  }
  auto AddStmt(Stmt stmt) -> StmtId {
    const StmtId id{static_cast<std::uint32_t>(stmts.size())};
    stmts.push_back(std::move(stmt));
    return id;
  }
  auto AddProceduralVar(ProceduralVarDecl decl) -> ProceduralVarId {
    const ProceduralVarId id{
        static_cast<std::uint32_t>(procedural_vars.size())};
    procedural_vars.push_back(std::move(decl));
    return id;
  }
};

}  // namespace lyra::hir
