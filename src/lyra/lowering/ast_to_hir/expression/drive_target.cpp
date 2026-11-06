#include "lyra/lowering/ast_to_hir/expression/drive_target.hpp"

#include "lyra/frontend/slang_source_span.hpp"
#include "lyra/lowering/ast_to_hir/expression/selects.hpp"
#include "slang/ast/expressions/SelectExpressions.h"

namespace lyra::lowering::ast_to_hir {

auto DriveTargetLowerer::LowerExpr(
    const slang::ast::Expression& expr, WalkFrame frame)
    -> diag::Result<hir::Expr> {
  if (&expr == clockvar_) return written_;
  if (const auto* sel = expr.as_if<slang::ast::ElementSelectExpression>()) {
    return LowerElementSelectExpr(
        *this, frame, *sel, frontend::SpanOf(expr.sourceRange));
  }
  if (const auto* sel = expr.as_if<slang::ast::RangeSelectExpression>()) {
    return LowerRangeSelectExpr(
        *this, frame, *sel, frontend::SpanOf(expr.sourceRange));
  }
  return proc_->LowerExpr(expr, frame);
}

}  // namespace lyra::lowering::ast_to_hir
