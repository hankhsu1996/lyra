#include "lyra/lowering/ast_to_hir/expression/inside.hpp"

#include <expected>
#include <utility>
#include <vector>

#include <slang/ast/Expression.h>
#include <slang/ast/expressions/OperatorExpressions.h>

#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/kind.hpp"
#include "lyra/lowering/ast_to_hir/expression/dispatch.hpp"
#include "lyra/lowering/ast_to_hir/module_lowerer.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerInsideExprProc(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::InsideExpression& in, const slang::ast::Expression& expr,
    diag::SourceSpan span) -> diag::Result<hir::Expr> {
  auto lhs_or = LowerProcExpr(proc, frame, in.left());
  if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
  const hir::ExprId lhs_id = proc.AddExpr(*std::move(lhs_or));

  std::vector<hir::InsideItem> items;
  items.reserve(in.rangeList().size());
  for (const auto* item : in.rangeList()) {
    auto item_or = LowerInsideItemImpl(proc, frame, *item);
    if (!item_or) return std::unexpected(std::move(item_or.error()));
    items.push_back(*std::move(item_or));
  }

  auto type_id = proc.Module().GetTypeIdOf(expr);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  return hir::Expr{
      .type = *type_id,
      .data = hir::InsideExpr{.lhs = lhs_id, .items = std::move(items)},
      .span = span,
  };
}

auto LowerInsideItemImpl(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::Expression& item_expr) -> diag::Result<hir::InsideItem> {
  auto& module = proc.Module();
  if (item_expr.kind == slang::ast::ExpressionKind::ValueRange) {
    const auto& vr = item_expr.as<slang::ast::ValueRangeExpression>();
    if (vr.rangeKind != slang::ast::ValueRangeKind::Simple) {
      return diag::Unsupported(
          module.SourceMapper().SpanOf(vr.sourceRange),
          diag::DiagCode::kUnsupportedExpressionForm,
          "tolerance-range form in inside operator is not yet supported",
          diag::UnsupportedCategory::kFeature);
    }
    auto lo_or = LowerProcExpr(proc, frame, vr.left());
    if (!lo_or) return std::unexpected(std::move(lo_or.error()));
    const hir::ExprId lo_id = proc.AddExpr(*std::move(lo_or));
    auto hi_or = LowerProcExpr(proc, frame, vr.right());
    if (!hi_or) return std::unexpected(std::move(hi_or.error()));
    const hir::ExprId hi_id = proc.AddExpr(*std::move(hi_or));
    return hir::InsideRangePair{.lo = lo_id, .hi = hi_id};
  }
  auto val_or = LowerProcExpr(proc, frame, item_expr);
  if (!val_or) return std::unexpected(std::move(val_or.error()));
  return proc.AddExpr(*std::move(val_or));
}

}  // namespace lyra::lowering::ast_to_hir
