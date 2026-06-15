#include "lyra/lowering/ast_to_hir/expression/operators.hpp"

#include <expected>
#include <utility>

#include <slang/ast/Expression.h>
#include <slang/ast/expressions/ConversionExpression.h>
#include <slang/ast/expressions/OperatorExpressions.h>

#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/kind.hpp"
#include "lyra/hir/conversion.hpp"
#include "lyra/lowering/ast_to_hir/expression/slang_atoms.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/structural_scope_lowerer.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerConversionExprProc(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::ConversionExpression& conv, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto operand_or = proc.LowerExpr(conv.operand(), frame);
  if (!operand_or) return std::unexpected(std::move(operand_or.error()));
  const hir::ExprId operand_id =
      frame.current_procedural_body->AddExpr(*std::move(operand_or));
  auto type_id = proc.Module().InternType(*conv.type, span);
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
    const slang::ast::UnaryExpression& un, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  const hir::UnaryOp op = LowerUnaryOp(un.op);
  auto operand_or = proc.LowerExpr(un.operand(), frame);
  if (!operand_or) return std::unexpected(std::move(operand_or.error()));
  const hir::ExprId operand_id =
      frame.current_procedural_body->AddExpr(*std::move(operand_or));
  auto type_id = proc.Module().InternType(*un.type, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  return hir::Expr{
      .type = *type_id,
      .data = hir::UnaryExpr{.op = op, .operand = operand_id},
      .span = span,
  };
}

auto LowerBinaryExprProc(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::BinaryExpression& bin, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto lhs_or = proc.LowerExpr(bin.left(), frame);
  if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
  const hir::ExprId lhs_id =
      frame.current_procedural_body->AddExpr(*std::move(lhs_or));
  auto rhs_or = proc.LowerExpr(bin.right(), frame);
  if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
  const hir::ExprId rhs_id =
      frame.current_procedural_body->AddExpr(*std::move(rhs_or));
  auto type_id = proc.Module().InternType(*bin.type, span);
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
    const slang::ast::ConditionalExpression& cond, diag::SourceSpan span)
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
  auto cond_or = proc.LowerExpr(*cond.conditions[0].expr, frame);
  if (!cond_or) return std::unexpected(std::move(cond_or.error()));
  const hir::ExprId cond_id =
      frame.current_procedural_body->AddExpr(*std::move(cond_or));
  auto then_or = proc.LowerExpr(cond.left(), frame);
  if (!then_or) return std::unexpected(std::move(then_or.error()));
  const hir::ExprId then_id =
      frame.current_procedural_body->AddExpr(*std::move(then_or));
  auto else_or = proc.LowerExpr(cond.right(), frame);
  if (!else_or) return std::unexpected(std::move(else_or.error()));
  const hir::ExprId else_id =
      frame.current_procedural_body->AddExpr(*std::move(else_or));
  auto type_id = proc.Module().InternType(*cond.type, span);
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
    StructuralScopeLowerer& scope, WalkFrame frame,
    const slang::ast::ConversionExpression& conv, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto operand_or = scope.LowerExpr(conv.operand(), frame);
  if (!operand_or) return std::unexpected(std::move(operand_or.error()));
  const hir::ExprId operand_id =
      frame.current_structural_scope->AddExpr(*std::move(operand_or));
  auto type_id = scope.Module().InternType(*conv.type, span);
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
    StructuralScopeLowerer& scope, WalkFrame frame,
    const slang::ast::UnaryExpression& un, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  const hir::UnaryOp op = LowerUnaryOp(un.op);
  auto operand_or = scope.LowerExpr(un.operand(), frame);
  if (!operand_or) return std::unexpected(std::move(operand_or.error()));
  const hir::ExprId operand_id =
      frame.current_structural_scope->AddExpr(*std::move(operand_or));
  auto type_id = scope.Module().InternType(*un.type, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  return hir::Expr{
      .type = *type_id,
      .data = hir::UnaryExpr{.op = op, .operand = operand_id},
      .span = span,
  };
}

auto LowerBinaryExprStructural(
    StructuralScopeLowerer& scope, WalkFrame frame,
    const slang::ast::BinaryExpression& bin, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto lhs_or = scope.LowerExpr(bin.left(), frame);
  if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
  const hir::ExprId lhs_id =
      frame.current_structural_scope->AddExpr(*std::move(lhs_or));
  auto rhs_or = scope.LowerExpr(bin.right(), frame);
  if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
  const hir::ExprId rhs_id =
      frame.current_structural_scope->AddExpr(*std::move(rhs_or));
  auto type_id = scope.Module().InternType(*bin.type, span);
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
    StructuralScopeLowerer& scope, WalkFrame frame,
    const slang::ast::ConditionalExpression& cond, diag::SourceSpan span)
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
  auto cond_or = scope.LowerExpr(*cond.conditions[0].expr, frame);
  if (!cond_or) return std::unexpected(std::move(cond_or.error()));
  const hir::ExprId cond_id =
      frame.current_structural_scope->AddExpr(*std::move(cond_or));
  auto then_or = scope.LowerExpr(cond.left(), frame);
  if (!then_or) return std::unexpected(std::move(then_or.error()));
  const hir::ExprId then_id =
      frame.current_structural_scope->AddExpr(*std::move(then_or));
  auto else_or = scope.LowerExpr(cond.right(), frame);
  if (!else_or) return std::unexpected(std::move(else_or.error()));
  const hir::ExprId else_id =
      frame.current_structural_scope->AddExpr(*std::move(else_or));
  auto type_id = scope.Module().InternType(*cond.type, span);
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
