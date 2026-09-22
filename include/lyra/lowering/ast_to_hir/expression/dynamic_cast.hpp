#pragma once

// Lowering of the LRM 6.24.2 dynamic cast (`$cast`), whose destination is
// written only where the assignment turns out to be valid.

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/ast_to_hir/expression/expr_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace slang::ast {
class CallExpression;
class Expression;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// Whether this expression is the construct below. The call is resolved apart
// from every other system subroutine, so the two places that must recognize it
// before lowering it ask here.
[[nodiscard]] auto IsDynamicCast(const slang::ast::Expression& expr) -> bool;

// The destination is an lvalue rather than a value, and it is written only when
// the answer is 1, so the call is resolved ahead of the argument loop: the
// generic loop hands every actual to whoever consumes the call, and here the
// first one is not an operand of anything.
//
// How an invalid assignment is handled is the caller's to supply, because LRM
// 6.24.2 settles it by which spelling the source used and the front end types
// both spellings alike.
template <ExprLowerer Lowerer>
auto LowerDynamicCastExpr(
    Lowerer& lowerer, WalkFrame frame, const slang::ast::CallExpression& call,
    hir::InvalidAssignmentHandling on_invalid, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;

// Lowers an expression standing somewhere nothing reads its answer. For every
// expression but one this changes nothing -- `i + 1` written as a statement
// means what it always means -- and the one is the construct above, so
// `on_invalid` reaches it and no other expression looks at it.
//
// What it changes there is which spelling the source used, because writing the
// answer down is what calling a subroutine as a function means (LRM 6.24.2): a
// call nothing reads is a task call. Two positions read no answer, a statement
// (LRM A.6.4) and a for-loop step (LRM A.6.8), and they differ in one thing. A
// statement may say it called the function anyway by writing the void cast that
// throws the answer away (LRM 6.24.1). A step may not, the grammar admitting a
// subroutine call there and no cast around one, so it is always a task call.
//
// The position decides this and not the expression, which is why a statement is
// not the whole of it.
template <ExprLowerer Lowerer>
auto LowerExprWithDiscardedAnswer(
    Lowerer& lowerer, WalkFrame frame, const slang::ast::Expression& expr,
    hir::InvalidAssignmentHandling on_invalid, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;

}  // namespace lyra::lowering::ast_to_hir
