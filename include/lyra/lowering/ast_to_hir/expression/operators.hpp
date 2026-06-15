#pragma once

// Lowering of unary, binary, conditional, and conversion expressions
// (LRM 11.4 operators).

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/structural_scope_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace slang::ast {
class BinaryExpression;
class ConditionalExpression;
class ConversionExpression;
class UnaryExpression;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// Procedural-context handlers.
auto LowerUnaryExprProc(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::UnaryExpression& un, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;
auto LowerBinaryExprProc(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::BinaryExpression& bin, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;
auto LowerConditionalExprProc(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::ConditionalExpression& cond, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;
auto LowerConversionExprProc(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::ConversionExpression& conv, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;

// Structural-context handlers.
auto LowerUnaryExprStructural(
    StructuralScopeLowerer& scope, WalkFrame frame,
    const slang::ast::UnaryExpression& un, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;
auto LowerBinaryExprStructural(
    StructuralScopeLowerer& scope, WalkFrame frame,
    const slang::ast::BinaryExpression& bin, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;
auto LowerConditionalExprStructural(
    StructuralScopeLowerer& scope, WalkFrame frame,
    const slang::ast::ConditionalExpression& cond, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;
auto LowerConversionExprStructural(
    StructuralScopeLowerer& scope, WalkFrame frame,
    const slang::ast::ConversionExpression& conv, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;

}  // namespace lyra::lowering::ast_to_hir
