#pragma once

// Lowering of the `inside` operator (LRM 11.4.13): its left operand and the
// members of its set, each as the source wrote it.

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/ast_to_hir/expression/expr_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace slang::ast {
class Expression;
class InsideExpression;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// The `inside` operator's meaning is independent of the enclosing scope, so one
// template over the pass class serves both contexts; explicit instantiations
// live in the implementation file.
template <ExprLowerer Lowerer>
auto LowerInsideExpr(
    Lowerer& lowerer, WalkFrame frame, const slang::ast::InsideExpression& in,
    diag::SourceSpan span) -> diag::Result<hir::Expr>;

}  // namespace lyra::lowering::ast_to_hir
