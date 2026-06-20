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
#include "lyra/hir/binary_op.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/expr_builders.hpp"
#include "lyra/hir/method.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/lowering/ast_to_hir/module_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/structural_scope_lowerer.hpp"

namespace lyra::lowering::ast_to_hir {

// LRM 7.10: `$` in a queue index or slice bound denotes the last element, i.e.
// `size(base) - 1`. The base whose `$` this resolves is threaded on the walk
// frame by the enclosing queue select; outside that context `$` has no value.
auto LowerUnboundedLiteralProc(
    ProcessLowerer& proc, WalkFrame frame, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  if (!frame.dollar_base.has_value()) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "`$` is only supported as a queue index or slice bound (LRM 7.10)",
        diag::UnsupportedCategory::kOperation);
  }
  auto& body = *frame.current_procedural_body;
  const hir::TypeId int32_type = proc.Module().Unit().builtins.int32;
  const hir::ExprId size_id = body.AddExpr(
      hir::Expr{
          .type = int32_type,
          .data =
              hir::CallExpr{
                  .callee = hir::SubroutineRef{hir::BuiltinMethodRef{
                      .method = hir::QueueMethodKind::kSize}},
                  .arguments = {*frame.dollar_base}},
          .span = span});
  const hir::ExprId one_id =
      body.AddExpr(hir::MakeInt32Literal(1, int32_type, span));
  return hir::Expr{
      .type = int32_type,
      .data =
          hir::BinaryExpr{
              .op = hir::BinaryOp::kSub, .lhs = size_id, .rhs = one_id},
      .span = span};
}

auto LowerElementSelectExprProc(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::ElementSelectExpression& sel, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  if (sel.value().type->getCanonicalType().isString()) {
    // LRM 6.16.3: a string is a byte sequence with no element lvalue, so an
    // index read is the getc query, not an addressed element select. The
    // write side lowers symmetrically to putc in the assignment path.
    auto base_or = proc.LowerExpr(sel.value(), frame);
    if (!base_or) return std::unexpected(std::move(base_or.error()));
    const hir::ExprId base_id =
        frame.current_procedural_body->AddExpr(*std::move(base_or));
    auto idx_or = proc.LowerExpr(sel.selector(), frame);
    if (!idx_or) return std::unexpected(std::move(idx_or.error()));
    const hir::ExprId idx_id =
        frame.current_procedural_body->AddExpr(*std::move(idx_or));
    auto type_id = proc.Module().InternType(*sel.type, span);
    if (!type_id) return std::unexpected(std::move(type_id.error()));
    return hir::Expr{
        .type = *type_id,
        .data =
            hir::CallExpr{
                .callee =
                    hir::BuiltinMethodRef{
                        .method = hir::StringMethodKind::kGetc},
                .arguments = {base_id, idx_id}},
        .span = span,
    };
  }
  if (!sel.value().type->isIntegral() && !sel.value().type->isUnpackedArray()) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "element-select on non-integral operand is not yet supported",
        diag::UnsupportedCategory::kOperation);
  }
  auto base_or = proc.LowerExpr(sel.value(), frame);
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const hir::ExprId base_id =
      frame.current_procedural_body->AddExpr(*std::move(base_or));

  const WalkFrame idx_frame =
      sel.value().type->isQueue() ? frame.WithDollarBase(base_id) : frame;
  auto idx_or = proc.LowerExpr(sel.selector(), idx_frame);
  if (!idx_or) return std::unexpected(std::move(idx_or.error()));
  const hir::ExprId idx_id =
      frame.current_procedural_body->AddExpr(*std::move(idx_or));

  auto type_id = proc.Module().InternType(*sel.type, span);
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
    const slang::ast::RangeSelectExpression& sel, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  if (!sel.value().type->isIntegral() && !sel.value().type->isUnpackedArray()) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "range-select on non-integral, non-unpacked operand is not yet "
        "supported",
        diag::UnsupportedCategory::kOperation);
  }

  auto base_or = proc.LowerExpr(sel.value(), frame);
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const hir::ExprId base_id =
      frame.current_procedural_body->AddExpr(*std::move(base_or));

  const WalkFrame bound_frame =
      sel.value().type->isQueue() ? frame.WithDollarBase(base_id) : frame;
  auto left_or = proc.LowerExpr(sel.left(), bound_frame);
  if (!left_or) return std::unexpected(std::move(left_or.error()));
  const hir::ExprId left_id =
      frame.current_procedural_body->AddExpr(*std::move(left_or));

  auto right_or = proc.LowerExpr(sel.right(), bound_frame);
  if (!right_or) return std::unexpected(std::move(right_or.error()));
  const hir::ExprId right_id =
      frame.current_procedural_body->AddExpr(*std::move(right_or));

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

  auto type_id = proc.Module().InternType(*sel.type, span);
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
    const slang::ast::MemberAccessExpression& sel, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  const auto* field = &sel.member.as<slang::ast::FieldSymbol>();
  if (sel.member.kind != slang::ast::SymbolKind::Field) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "member access target is not a struct field",
        diag::UnsupportedCategory::kOperation);
  }
  auto base_or = proc.LowerExpr(sel.value(), frame);
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const hir::ExprId base_id =
      frame.current_procedural_body->AddExpr(*std::move(base_or));
  auto type_id = proc.Module().InternType(*sel.type, span);
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
    StructuralScopeLowerer& scope, WalkFrame frame,
    const slang::ast::ElementSelectExpression& sel, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  if (!sel.value().type->isIntegral()) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedStructuralExpressionForm,
        "element-select on non-integral operand is not yet supported",
        diag::UnsupportedCategory::kOperation);
  }
  auto base_or = scope.LowerExpr(sel.value(), frame);
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const hir::ExprId base_id =
      frame.current_structural_scope->AddExpr(*std::move(base_or));
  auto idx_or = scope.LowerExpr(sel.selector(), frame);
  if (!idx_or) return std::unexpected(std::move(idx_or.error()));
  const hir::ExprId idx_id =
      frame.current_structural_scope->AddExpr(*std::move(idx_or));
  auto type_id = scope.Module().InternType(*sel.type, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  return hir::Expr{
      .type = *type_id,
      .data = hir::ElementSelectExpr{.base_value = base_id, .index = idx_id},
      .span = span,
  };
}

auto LowerRangeSelectExprStructural(
    StructuralScopeLowerer& scope, WalkFrame frame,
    const slang::ast::RangeSelectExpression& sel, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  if (!sel.value().type->isIntegral() && !sel.value().type->isUnpackedArray()) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedStructuralExpressionForm,
        "range-select on non-integral, non-unpacked operand is not yet "
        "supported",
        diag::UnsupportedCategory::kOperation);
  }
  auto base_or = scope.LowerExpr(sel.value(), frame);
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const hir::ExprId base_id =
      frame.current_structural_scope->AddExpr(*std::move(base_or));
  auto left_or = scope.LowerExpr(sel.left(), frame);
  if (!left_or) return std::unexpected(std::move(left_or.error()));
  const hir::ExprId left_id =
      frame.current_structural_scope->AddExpr(*std::move(left_or));
  auto right_or = scope.LowerExpr(sel.right(), frame);
  if (!right_or) return std::unexpected(std::move(right_or.error()));
  const hir::ExprId right_id =
      frame.current_structural_scope->AddExpr(*std::move(right_or));
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
  auto type_id = scope.Module().InternType(*sel.type, span);
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
    StructuralScopeLowerer& scope, WalkFrame frame,
    const slang::ast::MemberAccessExpression& sel, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  if (sel.member.kind != slang::ast::SymbolKind::Field) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedStructuralExpressionForm,
        "member access target is not a struct field",
        diag::UnsupportedCategory::kOperation);
  }
  const auto& field = sel.member.as<slang::ast::FieldSymbol>();
  auto base_or = scope.LowerExpr(sel.value(), frame);
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const hir::ExprId base_id =
      frame.current_structural_scope->AddExpr(*std::move(base_or));
  auto type_id = scope.Module().InternType(*sel.type, span);
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
