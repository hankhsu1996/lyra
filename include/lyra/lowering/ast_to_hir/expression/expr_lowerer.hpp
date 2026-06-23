#pragma once

#include <concepts>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace slang::ast {
class Expression;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

class ModuleLowerer;

// The duck-typed contract a pass class fulfils for context-free expression
// lowering: recurse into a sub-expression and reach the enclosing module. Both
// `ProcessLowerer` (procedural bodies) and `StructuralScopeLowerer` (structural
// scopes) satisfy it; the shared per-expression handler templates are
// constrained on it so the surface they depend on is named and checked rather
// than surfacing as a deep template error. Sub-expressions are interned through
// the walk frame's single expression arena, so the lowerer's own surface is
// only the recursion and the module. It is deliberately not a base class -- the
// two pass classes build different things and share no v-table
// (decisions/generic-lowering-machinery.md).
template <typename L>
concept ExprLowerer =
    requires(L& lowerer, const slang::ast::Expression& expr, WalkFrame frame) {
      {
        lowerer.LowerExpr(expr, frame)
      } -> std::same_as<diag::Result<hir::Expr>>;
      lowerer.Module();
    };

}  // namespace lyra::lowering::ast_to_hir
