#include "lyra/lowering/ast_to_hir/expression/selects.hpp"

#include <expected>
#include <utility>

#include <slang/ast/Expression.h>
#include <slang/ast/expressions/SelectExpressions.h>
#include <slang/ast/symbols/MemberSymbols.h>
#include <slang/ast/types/Type.h>
#include <slang/numeric/ConstantValue.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/kind.hpp"
#include "lyra/hir/binary_op.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/expr_builders.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/lowering/ast_to_hir/constant_value.hpp"
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
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "`$` is only supported as a queue index or slice bound (LRM 7.10)");
  }
  const hir::TypeId int32_type = proc.Module().Unit().builtins.int32;
  const hir::ExprId size_id = frame.Exprs().Add(
      hir::Expr{
          .type = int32_type,
          .data =
              hir::CallExpr{
                  .callee = hir::SubroutineRef{hir::BuiltinMethodRef{
                      .method = support::BuiltinFn::kSize}},
                  .arguments = {*frame.dollar_base}},
          .span = span});
  const hir::ExprId one_id =
      frame.Exprs().Add(hir::MakeInt32Literal(1, int32_type, span));
  return hir::Expr{
      .type = int32_type,
      .data =
          hir::BinaryExpr{
              .op = hir::BinaryOp::kSub, .lhs = size_id, .rhs = one_id},
      .span = span};
}

// The string base realizes as a `getc` query at HIR -> MIR (LRM 6.16.3); the
// queue base ($-index handling) is dynamic-only. slang rejects an element of a
// dynamic type outside procedural code, so those operands never arrive in a
// structural scope -- the guard and the $-base threading are uniform and the
// dynamic branches are simply dead there.
template <ExprLowerer Lowerer>
auto LowerElementSelectExpr(
    Lowerer& lowerer, WalkFrame frame,
    const slang::ast::ElementSelectExpression& sel, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  const auto& base_canonical = sel.value().type->getCanonicalType();
  if (!base_canonical.isString() && !sel.value().type->isIntegral() &&
      !sel.value().type->isUnpackedArray()) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "element-select on non-integral operand is not yet supported");
  }
  auto base_or = lowerer.LowerExpr(sel.value(), frame);
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const hir::ExprId base_id = frame.Exprs().Add(*std::move(base_or));

  const WalkFrame idx_frame =
      sel.value().type->isQueue() ? frame.WithDollarBase(base_id) : frame;
  auto idx_or = lowerer.LowerExpr(sel.selector(), idx_frame);
  if (!idx_or) return std::unexpected(std::move(idx_or.error()));
  const hir::ExprId idx_id = frame.Exprs().Add(*std::move(idx_or));

  auto type_id = lowerer.Module().InternType(*sel.type, span);
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

template <ExprLowerer Lowerer>
auto LowerRangeSelectExpr(
    Lowerer& lowerer, WalkFrame frame,
    const slang::ast::RangeSelectExpression& sel, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  if (!sel.value().type->isIntegral() && !sel.value().type->isUnpackedArray()) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "range-select on non-integral, non-unpacked operand is not yet "
        "supported");
  }

  auto base_or = lowerer.LowerExpr(sel.value(), frame);
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const hir::ExprId base_id = frame.Exprs().Add(*std::move(base_or));

  const WalkFrame bound_frame =
      sel.value().type->isQueue() ? frame.WithDollarBase(base_id) : frame;

  // A bound slang has evaluated to a constant (an `[msb:lsb]` endpoint or an
  // indexed-select width, both required constant by LRM 11.5.1) folds to a
  // literal from slang's value, so the downstream layer reads the constant
  // directly instead of re-deriving it from a structural parameter expression.
  // A non-constant bound -- a variable indexed-select base, a queue `$` --
  // falls back to structural lowering.
  auto lower_bound =
      [&](const slang::ast::Expression& bound) -> diag::Result<hir::ExprId> {
    if (const auto* constant = bound.getConstant(); constant != nullptr) {
      auto bound_type = lowerer.Module().InternType(*bound.type, span);
      if (!bound_type) return std::unexpected(std::move(bound_type.error()));
      auto literal = MakeConstantValueExpr(*constant, *bound_type, span);
      if (!literal) return std::unexpected(std::move(literal.error()));
      return frame.Exprs().Add(*std::move(literal));
    }
    auto lowered = lowerer.LowerExpr(bound, bound_frame);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    return frame.Exprs().Add(*std::move(lowered));
  };

  auto left_res = lower_bound(sel.left());
  if (!left_res) return std::unexpected(std::move(left_res.error()));
  const hir::ExprId left_id = *left_res;

  auto right_res = lower_bound(sel.right());
  if (!right_res) return std::unexpected(std::move(right_res.error()));
  const hir::ExprId right_id = *right_res;

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
        "LowerRangeSelectExpr: unknown slang RangeSelectionKind");
  }();

  auto type_id = lowerer.Module().InternType(*sel.type, span);
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

template <ExprLowerer Lowerer>
auto LowerMemberAccessExpr(
    Lowerer& lowerer, WalkFrame frame,
    const slang::ast::MemberAccessExpression& sel, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  if (sel.member.kind != slang::ast::SymbolKind::Field) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "member access target is not a struct field");
  }
  const auto* field = &sel.member.as<slang::ast::FieldSymbol>();
  auto base_or = lowerer.LowerExpr(sel.value(), frame);
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const hir::ExprId base_id = frame.Exprs().Add(*std::move(base_or));
  auto type_id = lowerer.Module().InternType(*sel.type, span);
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

// One concrete instantiation per pass class; the templates are defined here so
// the dispatchers in lower.cpp link against the symbols emitted in this file.
template auto LowerElementSelectExpr(
    ProcessLowerer&, WalkFrame, const slang::ast::ElementSelectExpression&,
    diag::SourceSpan) -> diag::Result<hir::Expr>;
template auto LowerElementSelectExpr(
    StructuralScopeLowerer&, WalkFrame,
    const slang::ast::ElementSelectExpression&, diag::SourceSpan)
    -> diag::Result<hir::Expr>;
template auto LowerRangeSelectExpr(
    ProcessLowerer&, WalkFrame, const slang::ast::RangeSelectExpression&,
    diag::SourceSpan) -> diag::Result<hir::Expr>;
template auto LowerRangeSelectExpr(
    StructuralScopeLowerer&, WalkFrame,
    const slang::ast::RangeSelectExpression&, diag::SourceSpan)
    -> diag::Result<hir::Expr>;
template auto LowerMemberAccessExpr(
    ProcessLowerer&, WalkFrame, const slang::ast::MemberAccessExpression&,
    diag::SourceSpan) -> diag::Result<hir::Expr>;
template auto LowerMemberAccessExpr(
    StructuralScopeLowerer&, WalkFrame,
    const slang::ast::MemberAccessExpression&, diag::SourceSpan)
    -> diag::Result<hir::Expr>;

}  // namespace lyra::lowering::ast_to_hir
