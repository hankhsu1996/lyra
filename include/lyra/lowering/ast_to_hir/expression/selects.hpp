#pragma once

// Lowering of access/projection expressions: ElementSelect (LRM 11.5.1),
// RangeSelect (LRM 11.5.1 part-select / indexed part-select), and
// MemberAccess (LRM 11.5.2 struct/union member access). Both procedural and
// structural contexts.

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/structural_scope_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace slang::ast {
class ElementSelectExpression;
class MemberAccessExpression;
class RangeSelectExpression;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// LRM 7.10 `$` (UnboundedLiteral) in a queue index / slice bound. Resolves to
// the queue's last index via the base bound on the walk frame.
auto LowerUnboundedLiteralProc(
    ProcessLowerer& proc, WalkFrame frame, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;

// Procedural-context handlers.
auto LowerElementSelectExprProc(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::ElementSelectExpression& sel, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;
auto LowerRangeSelectExprProc(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::RangeSelectExpression& sel, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;
auto LowerMemberAccessExprProc(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::MemberAccessExpression& sel, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;

// Structural-context handlers.
auto LowerElementSelectExprStructural(
    StructuralScopeLowerer& scope, WalkFrame frame,
    const slang::ast::ElementSelectExpression& sel, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;
auto LowerRangeSelectExprStructural(
    StructuralScopeLowerer& scope, WalkFrame frame,
    const slang::ast::RangeSelectExpression& sel, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;
auto LowerMemberAccessExprStructural(
    StructuralScopeLowerer& scope, WalkFrame frame,
    const slang::ast::MemberAccessExpression& sel, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;

}  // namespace lyra::lowering::ast_to_hir
