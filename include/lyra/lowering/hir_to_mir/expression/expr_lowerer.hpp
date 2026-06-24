#pragma once

#include <concepts>

#include "lyra/base/arena.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"

namespace lyra::lowering::hir_to_mir {

// The duck-typed contract a pass class fulfils for context-free expression
// lowering: recurse into a sub-expression (read or lhs context), reach the
// HIR expression arena, and reach the enclosing module. `ProcessLowerer`
// (procedural bodies) and `ClassLowerer` (structural scopes) both satisfy
// it; the shared per-expression handler templates are constrained on it so
// the surface they depend on is named and checked rather than surfacing as a
// deep template error. It is deliberately not a base class -- the two pass
// classes build different things and share no v-table.
template <typename L>
concept ExprLowerer =
    requires(L& lowerer, const hir::Expr& expr, WalkFrame frame) {
      {
        lowerer.LowerExpr(expr, frame)
      } -> std::same_as<diag::Result<mir::Expr>>;
      {
        lowerer.LowerLhsExpr(expr, frame)
      } -> std::same_as<diag::Result<mir::Expr>>;
      {
        lowerer.HirExprs()
      } -> std::convertible_to<const base::Arena<hir::Expr, hir::ExprId>&>;
      lowerer.Module();
    };

}  // namespace lyra::lowering::hir_to_mir
