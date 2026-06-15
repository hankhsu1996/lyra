#pragma once

// Lowering of the `inside` operator (LRM 11.4.13) and the per-item lowering
// shared with `case ... inside` (LRM 12.5.4) for both range and value items.

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/inside_item.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace slang::ast {
class Expression;
class InsideExpression;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

auto LowerInsideExprProc(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::InsideExpression& in, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;

// Lower one slang range_list entry. A ValueRange entry becomes an
// InsideRangePair; any other expression becomes a plain ExprId. Used by
// `inside` operator lowering and by `case ... inside` statement lowering.
auto LowerInsideItemImpl(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::Expression& item_expr) -> diag::Result<hir::InsideItem>;

}  // namespace lyra::lowering::ast_to_hir
