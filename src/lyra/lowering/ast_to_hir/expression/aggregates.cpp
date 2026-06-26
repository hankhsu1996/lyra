#include "lyra/lowering/ast_to_hir/expression/aggregates.hpp"

#include <expected>
#include <optional>
#include <utility>
#include <vector>

#include <slang/ast/Expression.h>
#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/expressions/OperatorExpressions.h>
#include <slang/ast/types/Type.h>

#include "lyra/diag/diag_code.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/lowering/ast_to_hir/module_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/structural_scope_lowerer.hpp"

namespace lyra::lowering::ast_to_hir {

template <ExprLowerer Lowerer>
auto LowerConcatExpr(
    Lowerer& lowerer, WalkFrame frame,
    const slang::ast::ConcatenationExpression& cc, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto& module = lowerer.Module();
  auto type_id = module.InternType(*cc.type, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  const auto kind = module.Unit().types.Get(*type_id).Kind();
  if (kind != hir::TypeKind::kString && kind != hir::TypeKind::kPackedArray &&
      kind != hir::TypeKind::kQueue) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "concatenation result type is not string, packed, or a queue (LRM "
        "11.4.12 / 10.10)");
  }
  std::vector<hir::ExprId> operand_ids;
  operand_ids.reserve(cc.operands().size());
  for (const auto* op : cc.operands()) {
    // LRM 11.4.12.1: a zero-multiplier replication contributes no bits to
    // the enclosing concatenation. Slang types such Replication nodes as
    // `void`; recognize that exact AST shape so we drop only the documented
    // zero-rep case and let any other unexpected void surface as an error
    // through the normal type-kind check downstream.
    if (op->kind == slang::ast::ExpressionKind::Replication &&
        op->type->isVoid()) {
      continue;
    }
    auto operand_or = lowerer.LowerExpr(*op, frame);
    if (!operand_or) return std::unexpected(std::move(operand_or.error()));
    operand_ids.push_back(frame.Exprs().Add(*std::move(operand_or)));
  }
  return hir::Expr{
      .type = *type_id,
      .data = hir::ConcatExpr{.operands = std::move(operand_ids)},
      .span = span,
  };
}

template <ExprLowerer Lowerer>
auto LowerReplicationExpr(
    Lowerer& lowerer, WalkFrame frame,
    const slang::ast::ReplicationExpression& rp, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto& module = lowerer.Module();
  auto type_id = module.InternType(*rp.type, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  const auto kind = module.Unit().types.Get(*type_id).Kind();
  if (kind != hir::TypeKind::kString && kind != hir::TypeKind::kPackedArray) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "replication result type is neither string nor packed "
        "(LRM 11.4.12.1)");
  }
  auto count_or = lowerer.LowerExpr(rp.count(), frame);
  if (!count_or) return std::unexpected(std::move(count_or.error()));
  const hir::ExprId count_id = frame.Exprs().Add(*std::move(count_or));
  auto concat_or = lowerer.LowerExpr(rp.concat(), frame);
  if (!concat_or) return std::unexpected(std::move(concat_or.error()));
  const hir::ExprId concat_id = frame.Exprs().Add(*std::move(concat_or));
  return hir::Expr{
      .type = *type_id,
      .data = hir::ReplicationExpr{.count = count_id, .concat = concat_id},
      .span = span,
  };
}

template <ExprLowerer Lowerer>
auto LowerAssignmentPatternFromElements(
    Lowerer& lowerer, WalkFrame frame,
    const slang::ast::AssignmentPatternExpressionBase& ap,
    diag::SourceSpan span) -> diag::Result<hir::Expr> {
  auto type_id = lowerer.Module().InternType(*ap.type, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  std::vector<hir::ExprId> element_ids;
  element_ids.reserve(ap.elements().size());
  for (const auto* elem : ap.elements()) {
    auto lowered = lowerer.LowerExpr(*elem, frame);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    element_ids.push_back(frame.Exprs().Add(*std::move(lowered)));
  }
  return hir::Expr{
      .type = *type_id,
      .data = hir::AssignmentPatternExpr{.elements = std::move(element_ids)},
      .span = span,
  };
}

template <ExprLowerer Lowerer>
auto LowerAssociativeAssignmentPattern(
    Lowerer& lowerer, WalkFrame frame,
    const slang::ast::StructuredAssignmentPatternExpression& ap,
    diag::SourceSpan span) -> diag::Result<hir::Expr> {
  auto type_id = lowerer.Module().InternType(*ap.type, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  std::vector<hir::AssociativeAssignmentPatternExpr::Entry> entries;
  entries.reserve(ap.indexSetters.size());
  for (const auto& setter : ap.indexSetters) {
    auto key_or = lowerer.LowerExpr(*setter.index, frame);
    if (!key_or) return std::unexpected(std::move(key_or.error()));
    const hir::ExprId key_id = frame.Exprs().Add(*std::move(key_or));
    auto value_or = lowerer.LowerExpr(*setter.expr, frame);
    if (!value_or) return std::unexpected(std::move(value_or.error()));
    const hir::ExprId value_id = frame.Exprs().Add(*std::move(value_or));
    entries.push_back({.key = key_id, .value = value_id});
  }
  std::optional<hir::ExprId> default_id;
  if (ap.defaultSetter != nullptr) {
    auto default_or = lowerer.LowerExpr(*ap.defaultSetter, frame);
    if (!default_or) return std::unexpected(std::move(default_or.error()));
    default_id = frame.Exprs().Add(*std::move(default_or));
  }
  return hir::Expr{
      .type = *type_id,
      .data =
          hir::AssociativeAssignmentPatternExpr{
              .entries = std::move(entries),
              .default_value = default_id,
          },
      .span = span,
  };
}

template <ExprLowerer Lowerer>
auto LowerReplicatedAssignmentPatternExpr(
    Lowerer& lowerer, WalkFrame frame,
    const slang::ast::ReplicatedAssignmentPatternExpression& rp,
    diag::SourceSpan span) -> diag::Result<hir::Expr> {
  auto type_id = lowerer.Module().InternType(*rp.type, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  auto count_or = lowerer.LowerExpr(rp.count(), frame);
  if (!count_or) return std::unexpected(std::move(count_or.error()));
  const hir::ExprId count_id = frame.Exprs().Add(*std::move(count_or));
  std::vector<hir::ExprId> item_ids;
  item_ids.reserve(rp.elements().size());
  for (const auto* elem : rp.elements()) {
    auto lowered = lowerer.LowerExpr(*elem, frame);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    item_ids.push_back(frame.Exprs().Add(*std::move(lowered)));
  }
  return hir::Expr{
      .type = *type_id,
      .data =
          hir::AssignmentPatternReplicationExpr{
              .count = count_id,
              .items = std::move(item_ids),
          },
      .span = span,
  };
}

// LRM 7.5.1 `new[N]` / `new[N](other)` dynamic-array constructor. Result type
// is the dynamic array type slang resolved for the expression; the optional
// initializer is `(other)` -- the LRM 7.5.1 source array for the copy-with-
// pad-or-truncate form.
auto LowerNewArrayExprProc(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::NewArrayExpression& na, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto type_id = proc.Module().InternType(*na.type, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  auto size_or = proc.LowerExpr(na.sizeExpr(), frame);
  if (!size_or) return std::unexpected(std::move(size_or.error()));
  const hir::ExprId size_id = frame.Exprs().Add(*std::move(size_or));
  std::optional<hir::ExprId> initializer_id;
  if (na.initExpr() != nullptr) {
    auto init_or = proc.LowerExpr(*na.initExpr(), frame);
    if (!init_or) return std::unexpected(std::move(init_or.error()));
    initializer_id = frame.Exprs().Add(*std::move(init_or));
  }
  return hir::Expr{
      .type = *type_id,
      .data =
          hir::DynamicArrayNewExpr{
              .size = size_id,
              .initializer = initializer_id,
          },
      .span = span,
  };
}

// One concrete instantiation per pass class; the templates are defined here so
// the dispatchers in lower.cpp link against the symbols emitted in this file.
template auto LowerConcatExpr(
    ProcessLowerer&, WalkFrame, const slang::ast::ConcatenationExpression&,
    diag::SourceSpan) -> diag::Result<hir::Expr>;
template auto LowerConcatExpr(
    StructuralScopeLowerer&, WalkFrame,
    const slang::ast::ConcatenationExpression&, diag::SourceSpan)
    -> diag::Result<hir::Expr>;
template auto LowerAssignmentPatternFromElements(
    ProcessLowerer&, WalkFrame,
    const slang::ast::AssignmentPatternExpressionBase&, diag::SourceSpan)
    -> diag::Result<hir::Expr>;
template auto LowerAssignmentPatternFromElements(
    StructuralScopeLowerer&, WalkFrame,
    const slang::ast::AssignmentPatternExpressionBase&, diag::SourceSpan)
    -> diag::Result<hir::Expr>;
template auto LowerReplicatedAssignmentPatternExpr(
    ProcessLowerer&, WalkFrame,
    const slang::ast::ReplicatedAssignmentPatternExpression&, diag::SourceSpan)
    -> diag::Result<hir::Expr>;
template auto LowerReplicatedAssignmentPatternExpr(
    StructuralScopeLowerer&, WalkFrame,
    const slang::ast::ReplicatedAssignmentPatternExpression&, diag::SourceSpan)
    -> diag::Result<hir::Expr>;
template auto LowerAssociativeAssignmentPattern(
    ProcessLowerer&, WalkFrame,
    const slang::ast::StructuredAssignmentPatternExpression&, diag::SourceSpan)
    -> diag::Result<hir::Expr>;
template auto LowerAssociativeAssignmentPattern(
    StructuralScopeLowerer&, WalkFrame,
    const slang::ast::StructuredAssignmentPatternExpression&, diag::SourceSpan)
    -> diag::Result<hir::Expr>;
template auto LowerReplicationExpr(
    ProcessLowerer&, WalkFrame, const slang::ast::ReplicationExpression&,
    diag::SourceSpan) -> diag::Result<hir::Expr>;
template auto LowerReplicationExpr(
    StructuralScopeLowerer&, WalkFrame,
    const slang::ast::ReplicationExpression&, diag::SourceSpan)
    -> diag::Result<hir::Expr>;

}  // namespace lyra::lowering::ast_to_hir
