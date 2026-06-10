#pragma once

// Lowering of unary, binary, conditional, and conversion expressions
// (LRM 11.4 operators).

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/scope_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace slang::ast {
class BinaryExpression;
class ConditionalExpression;
class ConversionExpression;
class Expression;
class UnaryExpression;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// Procedural-context handlers.
auto LowerUnaryExprProc(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::UnaryExpression& un, const slang::ast::Expression& expr,
    diag::SourceSpan span) -> diag::Result<hir::Expr>;
auto LowerBinaryExprProc(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::BinaryExpression& bin, const slang::ast::Expression& expr,
    diag::SourceSpan span) -> diag::Result<hir::Expr>;
auto LowerConditionalExprProc(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::ConditionalExpression& cond,
    const slang::ast::Expression& expr, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;
auto LowerConversionExprProc(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::ConversionExpression& conv,
    const slang::ast::Expression& expr, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;

// Structural-context handlers.
auto LowerUnaryExprStructural(
    ScopeLowerer& scope, WalkFrame frame, const slang::ast::UnaryExpression& un,
    const slang::ast::Expression& expr, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;
auto LowerBinaryExprStructural(
    ScopeLowerer& scope, WalkFrame frame,
    const slang::ast::BinaryExpression& bin, const slang::ast::Expression& expr,
    diag::SourceSpan span) -> diag::Result<hir::Expr>;
auto LowerConditionalExprStructural(
    ScopeLowerer& scope, WalkFrame frame,
    const slang::ast::ConditionalExpression& cond,
    const slang::ast::Expression& expr, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;
auto LowerConversionExprStructural(
    ScopeLowerer& scope, WalkFrame frame,
    const slang::ast::ConversionExpression& conv,
    const slang::ast::Expression& expr, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;

}  // namespace lyra::lowering::ast_to_hir
