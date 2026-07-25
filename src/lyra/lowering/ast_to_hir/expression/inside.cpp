#include "lyra/lowering/ast_to_hir/expression/inside.hpp"

#include <expected>
#include <utility>
#include <vector>

#include <slang/ast/Expression.h>
#include <slang/ast/expressions/OperatorExpressions.h>

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

  std::vector<hir::ExprId> items;
  items.reserve(in.rangeList().size());
  for (const auto* item : in.rangeList()) {
    auto item_or = lowerer.LowerExpr(*item, frame);
    if (!item_or) return std::unexpected(std::move(item_or.error()));
    items.push_back(frame.Exprs().Add(*std::move(item_or)));
  }

  auto type_id = lowerer.Owner().InternType(*in.type, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  return hir::Expr{
      .type = *type_id,
      .data = hir::InsideExpr{.lhs = lhs_id, .items = std::move(items)},
      .span = span,
  };
}

template auto LowerInsideExpr(
    ProcessLowerer& lowerer, WalkFrame frame,
    const slang::ast::InsideExpression& in, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;
template auto LowerInsideExpr(
    StructuralScopeLowerer& lowerer, WalkFrame frame,
    const slang::ast::InsideExpression& in, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;

}  // namespace lyra::lowering::ast_to_hir
