#pragma once

// Lowering of unary, binary, conditional, and conversion expressions
// (LRM 11.4 operators).

#include <span>

#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/ast_to_hir/expression/expr_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace slang::ast {
class BinaryExpression;
class ConditionalExpression;
class ConversionExpression;
class UnaryExpression;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// Lowers a `cond_predicate` (LRM 12.4): a `&&&`-separated list of conditions,
// as it appears in an `if` statement and the conditional `?:` operator. Without
// a `matches` pattern the predicate is the conjunction of its expressions, so
// the conditions fold into one HIR logical-AND returned as the single boolean
// expression the caller uses as its condition; the sugar reaches no lower
// layer. A `matches` pattern (LRM 12.6) is rejected with `pattern_code`, the
// caller's context-appropriate diagnostic. `Condition` is duck-typed over the
// statement's and the expression's structurally identical condition record.
template <ExprLowerer Lowerer, typename Condition>
auto LowerCondPredicate(
    Lowerer& lowerer, WalkFrame frame, std::span<const Condition> conditions,
    diag::DiagCode pattern_code, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;

// An operator's meaning is independent of the enclosing scope, so one template
// over the pass class serves both the procedural and structural contexts. The
// pass class is reached through the uniform `LowerExpr` recursion; explicit
// instantiations for the two pass classes live in the implementation file.
template <ExprLowerer Lowerer>
auto LowerUnaryExpr(
    Lowerer& lowerer, WalkFrame frame, const slang::ast::UnaryExpression& un,
    diag::SourceSpan span) -> diag::Result<hir::Expr>;
template <ExprLowerer Lowerer>
auto LowerBinaryExpr(
    Lowerer& lowerer, WalkFrame frame, const slang::ast::BinaryExpression& bin,
    diag::SourceSpan span) -> diag::Result<hir::Expr>;
template <ExprLowerer Lowerer>
auto LowerConditionalExpr(
    Lowerer& lowerer, WalkFrame frame,
    const slang::ast::ConditionalExpression& cond, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;
template <ExprLowerer Lowerer>
auto LowerConversionExpr(
    Lowerer& lowerer, WalkFrame frame,
    const slang::ast::ConversionExpression& conv, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;

}  // namespace lyra::lowering::ast_to_hir
