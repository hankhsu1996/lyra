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

#include "lyra/hir/conversion.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/ast_to_hir/expression/slang_atoms.hpp"
#include "lyra/lowering/ast_to_hir/pattern.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/structural_scope_lowerer.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

auto IsNull(const hir::TypePool& types, hir::TypeId type) -> bool {
  return types.Get(type).Is<hir::NullType>();
}

// Whether `op` compares its operands for equality, the comparisons LRM 8.4,
// 6.14 and 25.9 admit `null` to.
auto ComparesForEquality(slang::ast::BinaryOperator op) -> bool {
  using slang::ast::BinaryOperator;
  return op == BinaryOperator::Equality || op == BinaryOperator::Inequality ||
         op == BinaryOperator::CaseEquality ||
         op == BinaryOperator::CaseInequality;
}

}  // namespace

auto NullAsTypeOf(
    const hir::TypePool& types, const WalkFrame& frame, hir::ExprId operand,
    hir::TypeId type) -> hir::ExprId {
  const hir::Expr& expr = frame.Exprs().Get(operand);
  if (!IsNull(types, expr.type) || IsNull(types, type)) {
    return operand;
  }
  const diag::SourceSpan span = expr.span;
  return frame.Exprs().Add(
      hir::Expr{
          .type = type,
          .data =
              hir::ConversionExpr{
                  .kind = hir::ConversionKind::kImplicit, .operand = operand},
          .span = span});
}

auto ComparandsOf(
    const hir::TypePool& types, const WalkFrame& frame, hir::ExprId lhs,
    hir::ExprId rhs) -> Comparands {
  const hir::TypeId lhs_type = frame.Exprs().Get(lhs).type;
  const hir::TypeId rhs_type = frame.Exprs().Get(rhs).type;
  if (IsNull(types, lhs_type) && IsNull(types, rhs_type)) {
    const hir::TypeId chandle = types.Intern(hir::Type{hir::ChandleType{}});
    return Comparands{
        .lhs = NullAsTypeOf(types, frame, lhs, chandle),
        .rhs = NullAsTypeOf(types, frame, rhs, chandle)};
  }
  return Comparands{
      .lhs = NullAsTypeOf(types, frame, lhs, rhs_type),
      .rhs = NullAsTypeOf(types, frame, rhs, lhs_type)};
}

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
              .kind = LowerConversionKind(conv),
              .operand = operand_id,
          },
      .span = span,
  };
}

template <ExprLowerer Lowerer>
auto LowerUnaryExpr(
    Lowerer& lowerer, WalkFrame frame, const slang::ast::UnaryExpression& un,
    diag::SourceSpan span) -> diag::Result<hir::Expr> {
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
  auto lhs_or = lowerer.LowerExpr(bin.left(), frame);
  if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
  const hir::ExprId lhs_id = frame.Exprs().Add(*std::move(lhs_or));
  auto rhs_or = lowerer.LowerExpr(bin.right(), frame);
  if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
  const hir::ExprId rhs_id = frame.Exprs().Add(*std::move(rhs_or));
  const Comparands operands =
      ComparesForEquality(bin.op)
          ? ComparandsOf(lowerer.Owner().Unit().types, frame, lhs_id, rhs_id)
          : Comparands{.lhs = lhs_id, .rhs = rhs_id};
  auto type_id = lowerer.Owner().InternExpressionType(bin, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  return hir::Expr{
      .type = *type_id,
      .data =
          hir::BinaryExpr{
              .op = LowerBinaryOp(bin.op),
              .lhs = operands.lhs,
              .rhs = operands.rhs,
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
  auto else_or = lowerer.LowerExpr(cond.right(), frame);
  if (!else_or) return std::unexpected(std::move(else_or.error()));
  auto type_id = lowerer.Owner().InternExpressionType(cond, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  const hir::TypePool& types = lowerer.Owner().Unit().types;
  const hir::ExprId then_id = NullAsTypeOf(
      types, frame, frame.Exprs().Add(*std::move(then_or)), *type_id);
  const hir::ExprId else_id = NullAsTypeOf(
      types, frame, frame.Exprs().Add(*std::move(else_or)), *type_id);
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
