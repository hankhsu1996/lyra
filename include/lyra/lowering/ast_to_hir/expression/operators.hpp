#pragma once

// Lowering of unary, binary, conditional, and conversion expressions
// (LRM 11.4 operators).

#include <span>

#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/type_id.hpp"
#include "lyra/lowering/ast_to_hir/expression/expr_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace slang::ast {
class BinaryExpression;
class ConditionalExpression;
class ConversionExpression;
class UnaryExpression;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

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

// `operand` as a value of `type`: a `null` converted to it, and any other
// operand as it is. LRM 8.4, 6.14 and 25.9 make `null` a value of every type
// that refers, so which one it is is the type it meets, and the front end
// leaves it at a type of its own where it is compared with a value, chosen
// beside one, listed against one, or formatted.
auto NullAsTypeOf(
    const hir::TypePool& types, const WalkFrame& frame, hir::ExprId operand,
    hir::TypeId type) -> hir::ExprId;

// The two operands of a comparison, each a `null` brought to the other's type.
// Two `null`s compare as chandles do (LRM 6.14).
struct Comparands {
  hir::ExprId lhs;
  hir::ExprId rhs;
};
auto ComparandsOf(
    const hir::TypePool& types, const WalkFrame& frame, hir::ExprId lhs,
    hir::ExprId rhs) -> Comparands;

template <ExprLowerer Lowerer>
auto LowerConversionExpr(
    Lowerer& lowerer, WalkFrame frame,
    const slang::ast::ConversionExpression& conv, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;

}  // namespace lyra::lowering::ast_to_hir
