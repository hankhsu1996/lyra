#pragma once

// Lowering of access/projection expressions: ElementSelect (LRM 11.5.1),
// RangeSelect (LRM 11.5.1 part-select / indexed part-select), and
// MemberAccess (LRM 11.5.2 struct/union member access). Both procedural and
// structural contexts.

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/ast_to_hir/expression/expr_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace slang::ast {
class ElementSelectExpression;
class MemberAccessExpression;
class RangeSelectExpression;
class ClassPropertySymbol;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

class ProcessLowerer;

// LRM 7.10 `$` (UnboundedLiteral) in a queue index / slice bound. Resolves to
// the queue's last index via the base bound on the walk frame. A queue is a
// dynamic type, so `$` only ever appears in procedural code (LRM 7.10.1).
auto LowerUnboundedLiteralProc(
    ProcessLowerer& proc, WalkFrame frame, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;

// An access/projection's meaning is independent of the enclosing scope, so one
// template over the pass class serves both the procedural and structural
// contexts; explicit instantiations live in the implementation file.
template <ExprLowerer Lowerer>
auto LowerElementSelectExpr(
    Lowerer& lowerer, WalkFrame frame,
    const slang::ast::ElementSelectExpression& sel, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;
template <ExprLowerer Lowerer>
auto LowerRangeSelectExpr(
    Lowerer& lowerer, WalkFrame frame,
    const slang::ast::RangeSelectExpression& sel, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;
template <ExprLowerer Lowerer>
auto LowerMemberAccessExpr(
    Lowerer& lowerer, WalkFrame frame,
    const slang::ast::MemberAccessExpression& sel, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;

}  // namespace lyra::lowering::ast_to_hir
