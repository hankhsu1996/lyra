#include "lyra/lowering/ast_to_hir/expression/operators.hpp"

#include <expected>
#include <utility>

#include <slang/ast/Expression.h>
#include <slang/ast/expressions/ConversionExpression.h>
#include <slang/ast/expressions/OperatorExpressions.h>

#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/kind.hpp"
#include "lyra/hir/conversion.hpp"
#include "lyra/lowering/ast_to_hir/expression/dispatch.hpp"
#include "lyra/lowering/ast_to_hir/expression/slang_atoms.hpp"
#include "lyra/lowering/ast_to_hir/module_lowerer.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerConversionExprProc(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::ConversionExpression& conv,
    const slang::ast::Expression& expr, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto operand_or = LowerProcExpr(proc, frame, conv.operand());
  if (!operand_or) return std::unexpected(std::move(operand_or.error()));
  const hir::ExprId operand_id = proc.AddExpr(*std::move(operand_or));
  auto type_id = proc.Module().GetTypeIdOf(expr);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  return hir::Expr{
      .type = *type_id,
      .data =
          hir::ConversionExpr{
              .operand = operand_id,
              .kind = LowerConversionKind(conv.conversionKind),
          },
      .span = span,
  };
}

auto LowerUnaryExprProc(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::UnaryExpression& un, const slang::ast::Expression& expr,
    diag::SourceSpan span) -> diag::Result<hir::Expr> {
  const hir::UnaryOp op = LowerUnaryOp(un.op);
  auto operand_or = LowerProcExpr(proc, frame, un.operand());
  if (!operand_or) return std::unexpected(std::move(operand_or.error()));
  const hir::ExprId operand_id = proc.AddExpr(*std::move(operand_or));
  auto type_id = proc.Module().GetTypeIdOf(expr);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  return hir::Expr{
      .type = *type_id,
      .data = hir::UnaryExpr{.op = op, .operand = operand_id},
      .span = span,
  };
}

auto LowerBinaryExprProc(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::BinaryExpression& bin, const slang::ast::Expression& expr,
    diag::SourceSpan span) -> diag::Result<hir::Expr> {
  auto lhs_or = LowerProcExpr(proc, frame, bin.left());
  if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
  const hir::ExprId lhs_id = proc.AddExpr(*std::move(lhs_or));
  auto rhs_or = LowerProcExpr(proc, frame, bin.right());
  if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
  const hir::ExprId rhs_id = proc.AddExpr(*std::move(rhs_or));
  auto type_id = proc.Module().GetTypeIdOf(expr);
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

auto LowerConditionalExprProc(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::ConditionalExpression& cond,
    const slang::ast::Expression& expr, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  if (cond.conditions.size() != 1) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "conditional operator with `&&&` multi-condition is not yet "
        "supported",
        diag::UnsupportedCategory::kFeature);
  }
  if (cond.conditions[0].pattern != nullptr) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "conditional operator with `matches` pattern is not yet supported",
        diag::UnsupportedCategory::kFeature);
  }
  auto cond_or = LowerProcExpr(proc, frame, *cond.conditions[0].expr);
  if (!cond_or) return std::unexpected(std::move(cond_or.error()));
  const hir::ExprId cond_id = proc.AddExpr(*std::move(cond_or));
  auto then_or = LowerProcExpr(proc, frame, cond.left());
  if (!then_or) return std::unexpected(std::move(then_or.error()));
  const hir::ExprId then_id = proc.AddExpr(*std::move(then_or));
  auto else_or = LowerProcExpr(proc, frame, cond.right());
  if (!else_or) return std::unexpected(std::move(else_or.error()));
  const hir::ExprId else_id = proc.AddExpr(*std::move(else_or));
  auto type_id = proc.Module().GetTypeIdOf(expr);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  return hir::Expr{
      .type = *type_id,
      .data =
          hir::ConditionalExpr{
              .condition = cond_id,
              .then_value = then_id,
              .else_value = else_id,
          },
      .span = span,
  };
}

auto LowerConversionExprStructural(
    ScopeLowerer& scope, WalkFrame frame,
    const slang::ast::ConversionExpression& conv,
    const slang::ast::Expression& expr, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto operand_or = LowerStructuralExpr(scope, frame, conv.operand());
  if (!operand_or) return std::unexpected(std::move(operand_or.error()));
  const hir::ExprId operand_id = scope.AddExpr(*std::move(operand_or));
  auto type_id = scope.Module().GetTypeIdOf(expr);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  return hir::Expr{
      .type = *type_id,
      .data =
          hir::ConversionExpr{
              .operand = operand_id,
              .kind = LowerConversionKind(conv.conversionKind),
          },
      .span = span,
  };
}

auto LowerUnaryExprStructural(
    ScopeLowerer& scope, WalkFrame frame, const slang::ast::UnaryExpression& un,
    const slang::ast::Expression& expr, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  const hir::UnaryOp op = LowerUnaryOp(un.op);
  auto operand_or = LowerStructuralExpr(scope, frame, un.operand());
  if (!operand_or) return std::unexpected(std::move(operand_or.error()));
  const hir::ExprId operand_id = scope.AddExpr(*std::move(operand_or));
  auto type_id = scope.Module().GetTypeIdOf(expr);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  return hir::Expr{
      .type = *type_id,
      .data = hir::UnaryExpr{.op = op, .operand = operand_id},
      .span = span,
  };
}

auto LowerBinaryExprStructural(
    ScopeLowerer& scope, WalkFrame frame,
    const slang::ast::BinaryExpression& bin, const slang::ast::Expression& expr,
    diag::SourceSpan span) -> diag::Result<hir::Expr> {
  auto lhs_or = LowerStructuralExpr(scope, frame, bin.left());
  if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
  const hir::ExprId lhs_id = scope.AddExpr(*std::move(lhs_or));
  auto rhs_or = LowerStructuralExpr(scope, frame, bin.right());
  if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
  const hir::ExprId rhs_id = scope.AddExpr(*std::move(rhs_or));
  auto type_id = scope.Module().GetTypeIdOf(expr);
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

auto LowerConditionalExprStructural(
    ScopeLowerer& scope, WalkFrame frame,
    const slang::ast::ConditionalExpression& cond,
    const slang::ast::Expression& expr, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  if (cond.conditions.size() != 1) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedStructuralExpressionForm,
        "conditional operator with `&&&` multi-condition is not yet "
        "supported",
        diag::UnsupportedCategory::kFeature);
  }
  if (cond.conditions[0].pattern != nullptr) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedStructuralExpressionForm,
        "conditional operator with `matches` pattern is not yet supported",
        diag::UnsupportedCategory::kFeature);
  }
  auto cond_or = LowerStructuralExpr(scope, frame, *cond.conditions[0].expr);
  if (!cond_or) return std::unexpected(std::move(cond_or.error()));
  const hir::ExprId cond_id = scope.AddExpr(*std::move(cond_or));
  auto then_or = LowerStructuralExpr(scope, frame, cond.left());
  if (!then_or) return std::unexpected(std::move(then_or.error()));
  const hir::ExprId then_id = scope.AddExpr(*std::move(then_or));
  auto else_or = LowerStructuralExpr(scope, frame, cond.right());
  if (!else_or) return std::unexpected(std::move(else_or.error()));
  const hir::ExprId else_id = scope.AddExpr(*std::move(else_or));
  auto type_id = scope.Module().GetTypeIdOf(expr);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  return hir::Expr{
      .type = *type_id,
      .data =
          hir::ConditionalExpr{
              .condition = cond_id,
              .then_value = then_id,
              .else_value = else_id,
          },
      .span = span,
  };
}

}  // namespace lyra::lowering::ast_to_hir
