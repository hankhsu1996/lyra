#pragma once

// Lowering of CallExpression (LRM 13.x subroutine calls and 8.x method calls).
// Handles system subroutines, user-defined subroutines, type method calls, and
// the array-method `with`-clause closure construction.

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace slang::ast {
class CallExpression;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

auto LowerCallExprProc(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::CallExpression& call, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;

}  // namespace lyra::lowering::ast_to_hir
