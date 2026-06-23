#include "lyra/lowering/ast_to_hir/expression/operators.hpp"

#include <algorithm>
#include <expected>
#include <utility>

#include <slang/ast/Expression.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/expressions/ConversionExpression.h>
#include <slang/ast/expressions/OperatorExpressions.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/AllTypes.h>
#include <slang/ast/types/Type.h>

#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/kind.hpp"
#include "lyra/hir/conversion.hpp"
#include "lyra/lowering/ast_to_hir/expression/slang_atoms.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/structural_scope_lowerer.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// Whether `type` is a real-family type, or a composite (any depth) with a
// real-family leaf. Recurses the two ways a value type nests other value types:
// unpacked array elements and unpacked struct / union members. A packed
// struct / union cannot hold a real (packed members are integral); a class is a
// handle whose `===` compares identity, not members, so neither is recursed.
auto TypeHasRealLeaf(const slang::ast::Type& type) -> bool {
  const auto& canonical = type.getCanonicalType();
  if (canonical.kind == slang::ast::SymbolKind::FloatingType) {
    return true;
  }
  if (const auto* element = canonical.getArrayElementType();
      element != nullptr) {
    return TypeHasRealLeaf(*element);
  }
  if (canonical.isUnpackedStruct()) {
    return std::ranges::any_of(
        canonical.as<slang::ast::UnpackedStructType>().fields,
        [](const auto* field) { return TypeHasRealLeaf(field->getType()); });
  }
  if (canonical.isUnpackedUnion()) {
    return std::ranges::any_of(
        canonical.as<slang::ast::UnpackedUnionType>().fields,
        [](const auto* field) { return TypeHasRealLeaf(field->getType()); });
  }
  return false;
}

// LRM Table 11-1: case equality (`===` / `!==`) is not defined on real /
// shortreal operands. slang accepts the form and evaluates it as a value
// comparison, but that is a deviation from the standard the backend does not
// follow: for a real there is no x/z plane, so the bit-matching the case
// operators specify (LRM 12.5) has no single correct meaning. Lowering rejects
// it here -- including a real-element aggregate, where case equality would
// recurse element-wise into the same undefined comparison.
auto CheckBinaryOperands(
    const slang::ast::BinaryExpression& bin, diag::SourceSpan span)
    -> diag::Result<void> {
  const bool is_case_equality =
      bin.op == slang::ast::BinaryOperator::CaseEquality ||
      bin.op == slang::ast::BinaryOperator::CaseInequality;
  if (is_case_equality && (TypeHasRealLeaf(*bin.left().type) ||
                           TypeHasRealLeaf(*bin.right().type))) {
    return diag::Error(
        span, diag::DiagCode::kCaseEqualityOnRealOperand,
        "case equality (=== / !==) is not defined on real or shortreal "
        "operands (LRM Table 11-1)");
  }
  return {};
}

}  // namespace

auto LowerConversionExprProc(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::ConversionExpression& conv, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto operand_or = proc.LowerExpr(conv.operand(), frame);
  if (!operand_or) return std::unexpected(std::move(operand_or.error()));
  const hir::ExprId operand_id = frame.Exprs().Add(*std::move(operand_or));
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
  const hir::ExprId operand_id = frame.Exprs().Add(*std::move(operand_or));
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
  if (auto check = CheckBinaryOperands(bin, span); !check) {
    return std::unexpected(std::move(check.error()));
  }
  auto lhs_or = proc.LowerExpr(bin.left(), frame);
  if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
  const hir::ExprId lhs_id = frame.Exprs().Add(*std::move(lhs_or));
  auto rhs_or = proc.LowerExpr(bin.right(), frame);
  if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
  const hir::ExprId rhs_id = frame.Exprs().Add(*std::move(rhs_or));
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
        diag::UnsupportedCategory::kOperation);
  }
  if (cond.conditions[0].pattern != nullptr) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "conditional operator with `matches` pattern is not yet supported",
        diag::UnsupportedCategory::kOperation);
  }
  auto cond_or = proc.LowerExpr(*cond.conditions[0].expr, frame);
  if (!cond_or) return std::unexpected(std::move(cond_or.error()));
  const hir::ExprId cond_id = frame.Exprs().Add(*std::move(cond_or));
  auto then_or = proc.LowerExpr(cond.left(), frame);
  if (!then_or) return std::unexpected(std::move(then_or.error()));
  const hir::ExprId then_id = frame.Exprs().Add(*std::move(then_or));
  auto else_or = proc.LowerExpr(cond.right(), frame);
  if (!else_or) return std::unexpected(std::move(else_or.error()));
  const hir::ExprId else_id = frame.Exprs().Add(*std::move(else_or));
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
  const hir::ExprId operand_id = frame.Exprs().Add(*std::move(operand_or));
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
  const hir::ExprId operand_id = frame.Exprs().Add(*std::move(operand_or));
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
  if (auto check = CheckBinaryOperands(bin, span); !check) {
    return std::unexpected(std::move(check.error()));
  }
  auto lhs_or = scope.LowerExpr(bin.left(), frame);
  if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
  const hir::ExprId lhs_id = frame.Exprs().Add(*std::move(lhs_or));
  auto rhs_or = scope.LowerExpr(bin.right(), frame);
  if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
  const hir::ExprId rhs_id = frame.Exprs().Add(*std::move(rhs_or));
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
  const hir::ExprId cond_id = frame.Exprs().Add(*std::move(cond_or));
  auto then_or = scope.LowerExpr(cond.left(), frame);
  if (!then_or) return std::unexpected(std::move(then_or.error()));
  const hir::ExprId then_id = frame.Exprs().Add(*std::move(then_or));
  auto else_or = scope.LowerExpr(cond.right(), frame);
  if (!else_or) return std::unexpected(std::move(else_or.error()));
  const hir::ExprId else_id = frame.Exprs().Add(*std::move(else_or));
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
