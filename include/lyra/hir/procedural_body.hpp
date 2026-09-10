#pragma once

#include <cstdint>

#include "lyra/base/arena.hpp"
#include "lyra/base/registry.hpp"
#include "lyra/hir/assertion.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/pattern.hpp"
#include "lyra/hir/procedural_scope.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/stmt.hpp"

namespace lyra::hir {

// What procedural code is made of: the statements, expressions, patterns and
// local variables of one body, in the arenas its ids index into. A process, a
// subroutine, and the action of a concurrent assertion are all this.
//
// `root_scope` is the body's declaration-semantics entry -- the implicit root
// scope owning top-level locals and any directly-nested begin/end child scopes
// -- and belongs to the body because declarations do. Where execution *enters*
// does not: a procedure enters at one statement, and an assertion's action at
// whichever arm an outcome selects, so each owner states its own entries and
// none of them is a property of the body.
struct ProceduralBody {
  ProceduralScopeId root_scope{};
  base::Arena<Expr, ExprId> exprs;
  base::Arena<Stmt, StmtId> stmts;
  base::Arena<Pattern, PatternId> patterns;
  // The sequence and property trees of the concurrent assertions written in
  // this body. They sit beside the expressions rather than in the scope's
  // arenas because that is where the expressions they read are: a property
  // written in a procedure may name a static local of it (LRM 16.6).
  base::Arena<SequenceExpr, SequenceExprId> sequence_exprs;
  base::Arena<PropertyExpr, PropertyExprId> property_exprs;
  base::Registry<ProceduralVarDecl, ProceduralVarId> procedural_vars;
  std::uint32_t loop_label_count = 0;

  auto AddLoopLabel() -> LoopLabelId {
    return LoopLabelId{loop_label_count++};
  }
};

}  // namespace lyra::hir
