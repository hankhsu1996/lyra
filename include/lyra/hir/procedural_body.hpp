#pragma once

#include <cstdint>

#include "lyra/base/arena.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/procedural_scope.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/stmt.hpp"

namespace lyra::hir {

// The statement tree of a process or subroutine together with the arenas its
// statements, expressions, and local variables index into. Processes and
// subroutines are both procedural bodies; they differ only in what wraps the
// body (a process kind and sensitivity, or a subroutine signature).
//
// `root_stmt` is the body's execution-semantics entry; `root_scope` is the
// body's declaration-semantics entry (the implicit process / subroutine
// root scope that owns top-level locals and any directly-nested begin/end
// child scopes). The two views are linked by `BlockStmt.scope`, etc.,
// without either view duplicating the other.
struct ProceduralBody {
  StmtId root_stmt{};
  ProceduralScopeId root_scope{};
  base::Arena<Expr, ExprId> exprs;
  base::Arena<Stmt, StmtId> stmts;
  base::Arena<ProceduralVarDecl, ProceduralVarId> procedural_vars;
  std::uint32_t loop_label_count = 0;

  auto AddLoopLabel() -> LoopLabelId {
    return LoopLabelId{loop_label_count++};
  }
};

}  // namespace lyra::hir
