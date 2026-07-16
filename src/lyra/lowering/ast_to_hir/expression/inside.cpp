#include "lyra/lowering/ast_to_hir/expression/inside.hpp"

#include <expected>
#include <utility>
#include <vector>

#include <slang/ast/Expression.h>
#include <slang/ast/expressions/OperatorExpressions.h>

#include "lyra/diag/diag_code.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/structural_scope_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"

namespace lyra::lowering::ast_to_hir {

template <ExprLowerer Lowerer>
auto LowerInsideExpr(
    Lowerer& lowerer, WalkFrame frame, const slang::ast::InsideExpression& in,
    diag::SourceSpan span) -> diag::Result<hir::Expr> {
  auto lhs_or = lowerer.LowerExpr(in.left(), frame);
  if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
  const hir::ExprId lhs_id = frame.Exprs().Add(*std::move(lhs_or));

  std::vector<hir::InsideItem> items;
  items.reserve(in.rangeList().size());
  for (const auto* item : in.rangeList()) {
    auto item_or = LowerInsideItemImpl(lowerer, frame, *item);
    if (!item_or) return std::unexpected(std::move(item_or.error()));
    items.push_back(*std::move(item_or));
  }

  auto type_id = lowerer.Owner().InternType(*in.type, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  return hir::Expr{
      .type = *type_id,
      .data = hir::InsideExpr{.lhs = lhs_id, .items = std::move(items)},
      .span = span,
  };
}

template <ExprLowerer Lowerer>
auto LowerInsideItemImpl(
    Lowerer& lowerer, WalkFrame frame, const slang::ast::Expression& item_expr)
    -> diag::Result<hir::InsideItem> {
  auto& unit_lowerer = lowerer.Owner();
  if (item_expr.kind == slang::ast::ExpressionKind::ValueRange) {
    const auto& vr = item_expr.as<slang::ast::ValueRangeExpression>();
    if (vr.rangeKind != slang::ast::ValueRangeKind::Simple) {
      return diag::Fail(
          unit_lowerer.SourceMapper().SpanOf(vr.sourceRange),
          diag::DiagCode::kUnsupportedExpressionForm,
          "tolerance-range form in inside operator is not yet supported");
    }
    auto lo_or = lowerer.LowerExpr(vr.left(), frame);
    if (!lo_or) return std::unexpected(std::move(lo_or.error()));
    const hir::ExprId lo_id = frame.Exprs().Add(*std::move(lo_or));
    auto hi_or = lowerer.LowerExpr(vr.right(), frame);
    if (!hi_or) return std::unexpected(std::move(hi_or.error()));
    const hir::ExprId hi_id = frame.Exprs().Add(*std::move(hi_or));
    return hir::InsideRangePair{.lo = lo_id, .hi = hi_id};
  }
  auto val_or = lowerer.LowerExpr(item_expr, frame);
  if (!val_or) return std::unexpected(std::move(val_or.error()));
  return frame.Exprs().Add(*std::move(val_or));
}

template auto LowerInsideExpr(
    ProcessLowerer& lowerer, WalkFrame frame,
    const slang::ast::InsideExpression& in, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;
template auto LowerInsideExpr(
    StructuralScopeLowerer& lowerer, WalkFrame frame,
    const slang::ast::InsideExpression& in, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;
template auto LowerInsideItemImpl(
    ProcessLowerer& lowerer, WalkFrame frame,
    const slang::ast::Expression& item_expr) -> diag::Result<hir::InsideItem>;
template auto LowerInsideItemImpl(
    StructuralScopeLowerer& lowerer, WalkFrame frame,
    const slang::ast::Expression& item_expr) -> diag::Result<hir::InsideItem>;

}  // namespace lyra::lowering::ast_to_hir
