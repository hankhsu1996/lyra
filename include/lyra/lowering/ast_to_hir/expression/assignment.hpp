#pragma once

// Lowering of assignment-shaped expressions: Assignment (LRM 11.4.1) and
// IncDec (LRM 11.4.2 increment/decrement).

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/ast_to_hir/expression/expr_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace slang::ast {
class AssignmentExpression;
class UnaryExpression;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// What an assignment writes and what it writes there is the same question
// wherever it is written, so one template over the pass class serves both the
// procedural and structural contexts; explicit instantiations live in the
// implementation file. A loop generate's step is the structural one: LRM 27.4
// gives it a genvar with an assignment operator, or with an increment or a
// decrement, so both of these reach a construction that runs before the
// simulation does.
template <ExprLowerer Lowerer>
auto LowerAssignmentExpr(
    Lowerer& lowerer, WalkFrame frame,
    const slang::ast::AssignmentExpression& as, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;

template <ExprLowerer Lowerer>
auto LowerIncDecExpr(
    Lowerer& lowerer, WalkFrame frame, const slang::ast::UnaryExpression& un,
    diag::SourceSpan span) -> diag::Result<hir::Expr>;

}  // namespace lyra::lowering::ast_to_hir
