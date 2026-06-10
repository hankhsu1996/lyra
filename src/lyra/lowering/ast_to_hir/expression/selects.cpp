#include "lyra/lowering/ast_to_hir/expression/selects.hpp"

#include <expected>
#include <utility>

#include <slang/ast/Expression.h>
#include <slang/ast/expressions/SelectExpressions.h>
#include <slang/ast/symbols/MemberSymbols.h>
#include <slang/ast/types/Type.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/kind.hpp"
#include "lyra/lowering/ast_to_hir/expression/dispatch.hpp"
#include "lyra/lowering/ast_to_hir/module_lowerer.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerElementSelectExprProc(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::ElementSelectExpression& sel,
    const slang::ast::Expression& expr, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  if (!sel.value().type->isIntegral() && !sel.value().type->isUnpackedArray()) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "element-select on non-integral operand is not yet supported",
        diag::UnsupportedCategory::kOperation);
  }
  auto base_or = LowerProcExpr(proc, frame, sel.value());
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const hir::ExprId base_id = proc.AddExpr(*std::move(base_or));

  auto idx_or = LowerProcExpr(proc, frame, sel.selector());
  if (!idx_or) return std::unexpected(std::move(idx_or.error()));
  const hir::ExprId idx_id = proc.AddExpr(*std::move(idx_or));

  auto type_id = proc.Module().GetTypeIdOf(expr);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  return hir::Expr{
      .type = *type_id,
      .data =
          hir::ElementSelectExpr{
              .base_value = base_id,
              .index = idx_id,
          },
      .span = span,
  };
}

auto LowerRangeSelectExprProc(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::RangeSelectExpression& sel,
    const slang::ast::Expression& expr, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  if (!sel.value().type->isIntegral() && !sel.value().type->isUnpackedArray()) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "range-select on non-integral, non-unpacked operand is not yet "
        "supported",
        diag::UnsupportedCategory::kOperation);
  }

  auto base_or = LowerProcExpr(proc, frame, sel.value());
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const hir::ExprId base_id = proc.AddExpr(*std::move(base_or));

  auto left_or = LowerProcExpr(proc, frame, sel.left());
  if (!left_or) return std::unexpected(std::move(left_or.error()));
  const hir::ExprId left_id = proc.AddExpr(*std::move(left_or));

  auto right_or = LowerProcExpr(proc, frame, sel.right());
  if (!right_or) return std::unexpected(std::move(right_or.error()));
  const hir::ExprId right_id = proc.AddExpr(*std::move(right_or));

  hir::RangeBounds bounds = [&]() -> hir::RangeBounds {
    switch (sel.getSelectionKind()) {
      case slang::ast::RangeSelectionKind::Simple:
        return hir::RangeConstantBounds{
            .msb_expr = left_id, .lsb_expr = right_id};
      case slang::ast::RangeSelectionKind::IndexedUp:
        return hir::RangeIndexedUpBounds{
            .base_index = left_id, .width = right_id};
      case slang::ast::RangeSelectionKind::IndexedDown:
        return hir::RangeIndexedDownBounds{
            .base_index = left_id, .width = right_id};
    }
    throw InternalError(
        "LowerRangeSelectExprProc: unknown slang RangeSelectionKind");
  }();

  auto type_id = proc.Module().GetTypeIdOf(expr);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  return hir::Expr{
      .type = *type_id,
      .data =
          hir::RangeSelectExpr{
              .base_value = base_id,
              .bounds = std::move(bounds),
          },
      .span = span,
  };
}

auto LowerMemberAccessExprProc(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::MemberAccessExpression& sel,
    const slang::ast::Expression& expr, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  const auto* field = &sel.member.as<slang::ast::FieldSymbol>();
  if (sel.member.kind != slang::ast::SymbolKind::Field) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "member access target is not a struct field",
        diag::UnsupportedCategory::kOperation);
  }
  auto base_or = LowerProcExpr(proc, frame, sel.value());
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const hir::ExprId base_id = proc.AddExpr(*std::move(base_or));
  auto type_id = proc.Module().GetTypeIdOf(expr);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  return hir::Expr{
      .type = *type_id,
      .data =
          hir::MemberAccessExpr{
              .base_value = base_id,
              .field_index = field->fieldIndex,
          },
      .span = span,
  };
}

auto LowerElementSelectExprStructural(
    ScopeLowerer& scope, WalkFrame frame,
    const slang::ast::ElementSelectExpression& sel,
    const slang::ast::Expression& expr, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  if (!sel.value().type->isIntegral()) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedStructuralExpressionForm,
        "element-select on non-integral operand is not yet supported",
        diag::UnsupportedCategory::kOperation);
  }
  auto base_or = LowerStructuralExpr(scope, frame, sel.value());
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const hir::ExprId base_id = scope.AddExpr(*std::move(base_or));
  auto idx_or = LowerStructuralExpr(scope, frame, sel.selector());
  if (!idx_or) return std::unexpected(std::move(idx_or.error()));
  const hir::ExprId idx_id = scope.AddExpr(*std::move(idx_or));
  auto type_id = scope.Module().GetTypeIdOf(expr);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  return hir::Expr{
      .type = *type_id,
      .data = hir::ElementSelectExpr{.base_value = base_id, .index = idx_id},
      .span = span,
  };
}

auto LowerRangeSelectExprStructural(
    ScopeLowerer& scope, WalkFrame frame,
    const slang::ast::RangeSelectExpression& sel,
    const slang::ast::Expression& expr, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  if (!sel.value().type->isIntegral() && !sel.value().type->isUnpackedArray()) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedStructuralExpressionForm,
        "range-select on non-integral, non-unpacked operand is not yet "
        "supported",
        diag::UnsupportedCategory::kOperation);
  }
  auto base_or = LowerStructuralExpr(scope, frame, sel.value());
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const hir::ExprId base_id = scope.AddExpr(*std::move(base_or));
  auto left_or = LowerStructuralExpr(scope, frame, sel.left());
  if (!left_or) return std::unexpected(std::move(left_or.error()));
  const hir::ExprId left_id = scope.AddExpr(*std::move(left_or));
  auto right_or = LowerStructuralExpr(scope, frame, sel.right());
  if (!right_or) return std::unexpected(std::move(right_or.error()));
  const hir::ExprId right_id = scope.AddExpr(*std::move(right_or));
  hir::RangeBounds bounds = [&]() -> hir::RangeBounds {
    switch (sel.getSelectionKind()) {
      case slang::ast::RangeSelectionKind::Simple:
        return hir::RangeConstantBounds{
            .msb_expr = left_id, .lsb_expr = right_id};
      case slang::ast::RangeSelectionKind::IndexedUp:
        return hir::RangeIndexedUpBounds{
            .base_index = left_id, .width = right_id};
      case slang::ast::RangeSelectionKind::IndexedDown:
        return hir::RangeIndexedDownBounds{
            .base_index = left_id, .width = right_id};
    }
    throw InternalError(
        "LowerRangeSelectExprStructural: unknown slang RangeSelectionKind");
  }();
  auto type_id = scope.Module().GetTypeIdOf(expr);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  return hir::Expr{
      .type = *type_id,
      .data =
          hir::RangeSelectExpr{
              .base_value = base_id, .bounds = std::move(bounds)},
      .span = span,
  };
}

auto LowerMemberAccessExprStructural(
    ScopeLowerer& scope, WalkFrame frame,
    const slang::ast::MemberAccessExpression& sel,
    const slang::ast::Expression& expr, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  if (sel.member.kind != slang::ast::SymbolKind::Field) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedStructuralExpressionForm,
        "member access target is not a struct field",
        diag::UnsupportedCategory::kOperation);
  }
  const auto& field = sel.member.as<slang::ast::FieldSymbol>();
  auto base_or = LowerStructuralExpr(scope, frame, sel.value());
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const hir::ExprId base_id = scope.AddExpr(*std::move(base_or));
  auto type_id = scope.Module().GetTypeIdOf(expr);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  return hir::Expr{
      .type = *type_id,
      .data =
          hir::MemberAccessExpr{
              .base_value = base_id,
              .field_index = field.fieldIndex,
          },
      .span = span,
  };
}

}  // namespace lyra::lowering::ast_to_hir
