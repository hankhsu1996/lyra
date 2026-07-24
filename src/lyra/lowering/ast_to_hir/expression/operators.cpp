#include "lyra/lowering/ast_to_hir/expression/operators.hpp"

#include <algorithm>
#include <cstddef>
#include <expected>
#include <span>
#include <utility>

#include <slang/ast/Compilation.h>
#include <slang/ast/Expression.h>
#include <slang/ast/Scope.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/expressions/ConversionExpression.h>
#include <slang/ast/expressions/OperatorExpressions.h>
#include <slang/ast/statements/ConditionalStatements.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/AllTypes.h>
#include <slang/ast/types/Type.h>

#include "lyra/diag/diag_code.hpp"
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
    return diag::Fail(
        span, diag::DiagCode::kErrorCaseEqualityOnRealOperand,
        "case equality (=== / !==) is not defined on real or shortreal "
        "operands (LRM Table 11-1)");
  }
  return {};
}

}  // namespace

template <ExprLowerer Lowerer>
auto LowerConversionExpr(
    Lowerer& lowerer, WalkFrame frame,
    const slang::ast::ConversionExpression& conv, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto operand_or = lowerer.LowerExpr(conv.operand(), frame);
  if (!operand_or) return std::unexpected(std::move(operand_or.error()));
  const hir::ExprId operand_id = frame.Exprs().Add(*std::move(operand_or));
  auto type_id = lowerer.Owner().InternType(*conv.type, span);
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

template <ExprLowerer Lowerer>
auto LowerUnaryExpr(
    Lowerer& lowerer, WalkFrame frame, const slang::ast::UnaryExpression& un,
    diag::SourceSpan span) -> diag::Result<hir::Expr> {
  const hir::UnaryOp op = LowerUnaryOp(un.op);
  auto operand_or = lowerer.LowerExpr(un.operand(), frame);
  if (!operand_or) return std::unexpected(std::move(operand_or.error()));
  const hir::ExprId operand_id = frame.Exprs().Add(*std::move(operand_or));
  auto type_id = lowerer.Owner().InternType(*un.type, span);
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
  if (auto check = CheckBinaryOperands(bin, span); !check) {
    return std::unexpected(std::move(check.error()));
  }
  auto lhs_or = lowerer.LowerExpr(bin.left(), frame);
  if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
  const hir::ExprId lhs_id = frame.Exprs().Add(*std::move(lhs_or));
  auto rhs_or = lowerer.LowerExpr(bin.right(), frame);
  if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
  const hir::ExprId rhs_id = frame.Exprs().Add(*std::move(rhs_or));
  auto type_id = lowerer.Owner().InternType(*bin.type, span);
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

template <ExprLowerer Lowerer, typename Condition>
auto LowerCondPredicate(
    Lowerer& lowerer, WalkFrame frame, std::span<const Condition> conditions,
    diag::DiagCode pattern_code, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  bool any_four_state = false;
  for (const auto& condition : conditions) {
    if (condition.pattern != nullptr) {
      return diag::Fail(
          span, pattern_code,
          "pattern matching in a condition is not yet supported");
    }
    any_four_state = any_four_state || condition.expr->type->isFourState();
  }

  auto first_or = lowerer.LowerExpr(*conditions.front().expr, frame);
  if (!first_or) return std::unexpected(std::move(first_or.error()));
  if (conditions.size() == 1) {
    return *std::move(first_or);
  }

  // A synthesized `&&` carries slang's own logical-result type: a 1-bit `bit`,
  // or `logic` when any operand is four-state so an X / Z operand still
  // propagates before the condition reduces it to false.
  const slang::ast::Compilation& compilation =
      lowerer.Owner().SourceScope().getCompilation();
  auto bit1_or = lowerer.Owner().InternType(
      any_four_state ? compilation.getLogicType() : compilation.getBitType(),
      span);
  if (!bit1_or) return std::unexpected(std::move(bit1_or.error()));

  const auto make_and = [&](hir::ExprId lhs, hir::ExprId rhs) -> hir::Expr {
    return hir::Expr{
        .type = *bit1_or,
        .data =
            hir::BinaryExpr{
                .op = hir::BinaryOp::kLogicalAnd, .lhs = lhs, .rhs = rhs},
        .span = span};
  };

  // Fold every condition but the last into the accumulator, interning each
  // intermediate so the next `&&` can name it; the last condition forms the
  // returned expression as data, which the caller interns.
  hir::ExprId acc = frame.Exprs().Add(*std::move(first_or));
  for (std::size_t i = 1; i + 1 < conditions.size(); ++i) {
    auto rhs_or = lowerer.LowerExpr(*conditions[i].expr, frame);
    if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
    const hir::ExprId rhs = frame.Exprs().Add(*std::move(rhs_or));
    acc = frame.Exprs().Add(make_and(acc, rhs));
  }
  auto last_or = lowerer.LowerExpr(*conditions.back().expr, frame);
  if (!last_or) return std::unexpected(std::move(last_or.error()));
  const hir::ExprId last = frame.Exprs().Add(*std::move(last_or));
  return make_and(acc, last);
}

template <ExprLowerer Lowerer>
auto LowerConditionalExpr(
    Lowerer& lowerer, WalkFrame frame,
    const slang::ast::ConditionalExpression& cond, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto cond_or = LowerCondPredicate(
      lowerer, frame, cond.conditions,
      diag::DiagCode::kUnsupportedExpressionForm, span);
  if (!cond_or) return std::unexpected(std::move(cond_or.error()));
  const hir::ExprId cond_id = frame.Exprs().Add(*std::move(cond_or));
  auto then_or = lowerer.LowerExpr(cond.left(), frame);
  if (!then_or) return std::unexpected(std::move(then_or.error()));
  const hir::ExprId then_id = frame.Exprs().Add(*std::move(then_or));
  auto else_or = lowerer.LowerExpr(cond.right(), frame);
  if (!else_or) return std::unexpected(std::move(else_or.error()));
  const hir::ExprId else_id = frame.Exprs().Add(*std::move(else_or));
  auto type_id = lowerer.Owner().InternType(*cond.type, span);
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
template auto LowerCondPredicate(
    ProcessLowerer&, WalkFrame,
    std::span<const slang::ast::ConditionalStatement::Condition>,
    diag::DiagCode, diag::SourceSpan) -> diag::Result<hir::Expr>;
template auto LowerCondPredicate(
    ProcessLowerer&, WalkFrame,
    std::span<const slang::ast::ConditionalExpression::Condition>,
    diag::DiagCode, diag::SourceSpan) -> diag::Result<hir::Expr>;
template auto LowerCondPredicate(
    StructuralScopeLowerer&, WalkFrame,
    std::span<const slang::ast::ConditionalExpression::Condition>,
    diag::DiagCode, diag::SourceSpan) -> diag::Result<hir::Expr>;

}  // namespace lyra::lowering::ast_to_hir
