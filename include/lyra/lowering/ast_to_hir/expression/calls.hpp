#pragma once

// Lowering of CallExpression (LRM 13.x subroutine calls and 8.x method calls).
// Handles system subroutines, user-defined subroutines, type method calls, and
// the array-method `with`-clause closure construction.

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/ast_to_hir/expression/expr_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace slang::ast {
class CallExpression;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// A call's meaning is independent of the enclosing scope, so one template over
// the pass class serves both the procedural and structural contexts; explicit
// instantiations live in the implementation file. The LRM 7.12 `with`-clause
// iteration element is the clause's own binding, reached without the enclosing
// scope's variable storage, so nothing here is procedural-only.
template <ExprLowerer Lowerer>
auto LowerCallExpr(
    Lowerer& lowerer, WalkFrame frame, const slang::ast::CallExpression& call,
    diag::SourceSpan span) -> diag::Result<hir::Expr>;

}  // namespace lyra::lowering::ast_to_hir
