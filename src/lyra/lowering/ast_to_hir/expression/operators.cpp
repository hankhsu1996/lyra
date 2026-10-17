#include "lyra/lowering/ast_to_hir/expression/operators.hpp"

#include <expected>
#include <utility>

#include <slang/ast/Compilation.h>
#include <slang/ast/Expression.h>
#include <slang/ast/Scope.h>
#include <slang/ast/expressions/ConversionExpression.h>
#include <slang/ast/expressions/OperatorExpressions.h>
#include <slang/ast/statements/ConditionalStatements.h>
#include <slang/ast/types/Type.h>

#include "lyra/lowering/ast_to_hir/event_handle.hpp"
#include "lyra/lowering/ast_to_hir/expression/slang_atoms.hpp"
#include "lyra/lowering/ast_to_hir/pattern.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/structural_scope_lowerer.hpp"

namespace lyra::lowering::ast_to_hir {

template <ExprLowerer Lowerer>
auto LowerConversionExpr(
    Lowerer& lowerer, WalkFrame frame,
    const slang::ast::ConversionExpression& conv, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto operand_or = lowerer.LowerExpr(conv.operand(), frame);
  if (!operand_or) return std::unexpected(std::move(operand_or.error()));
  const hir::ExprId operand_id = frame.Exprs().Add(*std::move(operand_or));
  auto type_id = lowerer.Owner().InternExpressionType(conv, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  return hir::Expr{
      .type = *type_id,
      .data =
          hir::ConversionExpr{
              .kind = LowerConversionKind(conv.conversionKind),
              .operand = operand_id,
          },
      .span = span,
  };
}

template <ExprLowerer Lowerer>
auto LowerUnaryExpr(
    Lowerer& lowerer, WalkFrame frame, const slang::ast::UnaryExpression& un,
    diag::SourceSpan span) -> diag::Result<hir::Expr> {
  if (auto refused = RefuseReadingAnEventAsAValue(*un.operand().type, span);
      !refused) {
    return std::unexpected(std::move(refused.error()));
  }
  const hir::UnaryOp op = LowerUnaryOp(un.op);
  auto operand_or = lowerer.LowerExpr(un.operand(), frame);
  if (!operand_or) return std::unexpected(std::move(operand_or.error()));
  const hir::ExprId operand_id = frame.Exprs().Add(*std::move(operand_or));
  auto type_id = lowerer.Owner().InternExpressionType(un, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  return hir::Expr{
      .type = *type_id,
      .data = hir::UnaryExpr{.op = op, .operand = operand_id},
      .span = span,
  };
}

template <ExprLowerer Lowerer>
auto LowerBinaryExpr(
    Lowerer& lowerer, WalkFrame frame, const slang::ast::BinaryExpression& bin,
    diag::SourceSpan span) -> diag::Result<hir::Expr> {
  if (auto refused = RefuseReadingAnEventAsAValue(*bin.left().type, span);
      !refused) {
    return std::unexpected(std::move(refused.error()));
  }
  if (auto refused = RefuseReadingAnEventAsAValue(*bin.right().type, span);
      !refused) {
    return std::unexpected(std::move(refused.error()));
  }
  auto lhs_or = lowerer.LowerExpr(bin.left(), frame);
  if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
  const hir::ExprId lhs_id = frame.Exprs().Add(*std::move(lhs_or));
  auto rhs_or = lowerer.LowerExpr(bin.right(), frame);
  if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
  const hir::ExprId rhs_id = frame.Exprs().Add(*std::move(rhs_or));
  auto type_id = lowerer.Owner().InternExpressionType(bin, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  return hir::Expr{
      .type = *type_id,
      .data =
          hir::BinaryExpr{
              .op = LowerBinaryOp(bin.op),
              .lhs = lhs_id,
              .rhs = rhs_id,
          },
      .span = span,
  };
}

template <ExprLowerer Lowerer>
auto LowerConditionalExpr(
    Lowerer& lowerer, WalkFrame frame,
    const slang::ast::ConditionalExpression& cond, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto clauses_or =
      LowerConditionClauses(lowerer, frame, cond.conditions, span);
  if (!clauses_or) return std::unexpected(std::move(clauses_or.error()));
  auto then_or = lowerer.LowerExpr(cond.left(), frame);
  if (!then_or) return std::unexpected(std::move(then_or.error()));
  const hir::ExprId then_id = frame.Exprs().Add(*std::move(then_or));
  auto else_or = lowerer.LowerExpr(cond.right(), frame);
  if (!else_or) return std::unexpected(std::move(else_or.error()));
  const hir::ExprId else_id = frame.Exprs().Add(*std::move(else_or));
  auto type_id = lowerer.Owner().InternExpressionType(cond, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  return hir::Expr{
      .type = *type_id,
      .data =
          hir::ConditionalExpr{
              .conditions = *std::move(clauses_or),
              .then_value = then_id,
              .else_value = else_id,
          },
      .span = span,
  };
}

// One concrete instantiation per pass class. The handler templates are defined
// in this file rather than the header so the file-local helpers stay private;
// the dispatchers in lower.cpp link against the symbols emitted here.
template auto LowerConversionExpr(
    ProcessLowerer&, WalkFrame, const slang::ast::ConversionExpression&,
    diag::SourceSpan) -> diag::Result<hir::Expr>;
template auto LowerConversionExpr(
    StructuralScopeLowerer&, WalkFrame, const slang::ast::ConversionExpression&,
    diag::SourceSpan) -> diag::Result<hir::Expr>;
template auto LowerUnaryExpr(
    ProcessLowerer&, WalkFrame, const slang::ast::UnaryExpression&,
    diag::SourceSpan) -> diag::Result<hir::Expr>;
template auto LowerUnaryExpr(
    StructuralScopeLowerer&, WalkFrame, const slang::ast::UnaryExpression&,
    diag::SourceSpan) -> diag::Result<hir::Expr>;
template auto LowerBinaryExpr(
    ProcessLowerer&, WalkFrame, const slang::ast::BinaryExpression&,
    diag::SourceSpan) -> diag::Result<hir::Expr>;
template auto LowerBinaryExpr(
    StructuralScopeLowerer&, WalkFrame, const slang::ast::BinaryExpression&,
    diag::SourceSpan) -> diag::Result<hir::Expr>;
template auto LowerConditionalExpr(
    ProcessLowerer&, WalkFrame, const slang::ast::ConditionalExpression&,
    diag::SourceSpan) -> diag::Result<hir::Expr>;
template auto LowerConditionalExpr(
    StructuralScopeLowerer&, WalkFrame,
    const slang::ast::ConditionalExpression&, diag::SourceSpan)
    -> diag::Result<hir::Expr>;

}  // namespace lyra::lowering::ast_to_hir
