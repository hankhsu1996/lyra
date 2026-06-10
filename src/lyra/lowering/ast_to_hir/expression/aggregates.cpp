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
#include "lyra/diag/kind.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/lowering/ast_to_hir/expression/dispatch.hpp"
#include "lyra/lowering/ast_to_hir/module_lowerer.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerConcatExprProc(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::ConcatenationExpression& cc,
    const slang::ast::Expression& expr, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto& module = proc.Module();
  auto type_id = module.GetTypeIdOf(expr);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  const auto kind = module.GetType(*type_id).Kind();
  if (kind != hir::TypeKind::kString && kind != hir::TypeKind::kPackedArray) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "concatenation result type is neither string nor packed (LRM 11.4.12)",
        diag::UnsupportedCategory::kOperation);
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
    auto operand_or = LowerProcExpr(proc, frame, *op);
    if (!operand_or) return std::unexpected(std::move(operand_or.error()));
    operand_ids.push_back(proc.AddExpr(*std::move(operand_or)));
  }
  return hir::Expr{
      .type = *type_id,
      .data = hir::ConcatExpr{.operands = std::move(operand_ids)},
      .span = span,
  };
}

auto LowerReplicationExprProc(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::ReplicationExpression& rp,
    const slang::ast::Expression& expr, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto& module = proc.Module();
  auto type_id = module.GetTypeIdOf(expr);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  const auto kind = module.GetType(*type_id).Kind();
  if (kind != hir::TypeKind::kString && kind != hir::TypeKind::kPackedArray) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "replication result type is neither string nor packed "
        "(LRM 11.4.12.1)",
        diag::UnsupportedCategory::kOperation);
  }
  auto count_or = LowerProcExpr(proc, frame, rp.count());
  if (!count_or) return std::unexpected(std::move(count_or.error()));
  const hir::ExprId count_id = proc.AddExpr(*std::move(count_or));
  auto concat_or = LowerProcExpr(proc, frame, rp.concat());
  if (!concat_or) return std::unexpected(std::move(concat_or.error()));
  const hir::ExprId concat_id = proc.AddExpr(*std::move(concat_or));
  return hir::Expr{
      .type = *type_id,
      .data = hir::ReplicationExpr{.count = count_id, .concat = concat_id},
      .span = span,
  };
}

auto LowerAssignmentPatternFromElementsProc(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::AssignmentPatternExpressionBase& ap,
    const slang::ast::Expression& expr, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto type_id = proc.Module().GetTypeIdOf(expr);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  std::vector<hir::ExprId> element_ids;
  element_ids.reserve(ap.elements().size());
  for (const auto* elem : ap.elements()) {
    auto lowered = LowerProcExpr(proc, frame, *elem);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    element_ids.push_back(proc.AddExpr(*std::move(lowered)));
  }
  return hir::Expr{
      .type = *type_id,
      .data = hir::AssignmentPatternExpr{.elements = std::move(element_ids)},
      .span = span,
  };
}

auto LowerReplicatedAssignmentPatternExprProc(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::ReplicatedAssignmentPatternExpression& rp,
    const slang::ast::Expression& expr, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto type_id = proc.Module().GetTypeIdOf(expr);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  auto count_or = LowerProcExpr(proc, frame, rp.count());
  if (!count_or) return std::unexpected(std::move(count_or.error()));
  const hir::ExprId count_id = proc.AddExpr(*std::move(count_or));
  std::vector<hir::ExprId> item_ids;
  item_ids.reserve(rp.elements().size());
  for (const auto* elem : rp.elements()) {
    auto lowered = LowerProcExpr(proc, frame, *elem);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    item_ids.push_back(proc.AddExpr(*std::move(lowered)));
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
    const slang::ast::NewArrayExpression& na,
    const slang::ast::Expression& expr, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto type_id = proc.Module().GetTypeIdOf(expr);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  auto size_or = LowerProcExpr(proc, frame, na.sizeExpr());
  if (!size_or) return std::unexpected(std::move(size_or.error()));
  const hir::ExprId size_id = proc.AddExpr(*std::move(size_or));
  std::optional<hir::ExprId> initializer_id;
  if (na.initExpr() != nullptr) {
    auto init_or = LowerProcExpr(proc, frame, *na.initExpr());
    if (!init_or) return std::unexpected(std::move(init_or.error()));
    initializer_id = proc.AddExpr(*std::move(init_or));
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

auto LowerConcatExprStructural(
    ScopeLowerer& scope, WalkFrame frame,
    const slang::ast::ConcatenationExpression& cc,
    const slang::ast::Expression& expr, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto& module = scope.Module();
  auto type_id = module.GetTypeIdOf(expr);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  const auto kind = module.GetType(*type_id).Kind();
  if (kind != hir::TypeKind::kString && kind != hir::TypeKind::kPackedArray) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedStructuralExpressionForm,
        "concatenation result type is neither string nor packed (LRM 11.4.12)",
        diag::UnsupportedCategory::kOperation);
  }
  std::vector<hir::ExprId> operand_ids;
  operand_ids.reserve(cc.operands().size());
  for (const auto* op : cc.operands()) {
    if (op->kind == slang::ast::ExpressionKind::Replication &&
        op->type->isVoid()) {
      continue;
    }
    auto operand_or = LowerStructuralExpr(scope, frame, *op);
    if (!operand_or) return std::unexpected(std::move(operand_or.error()));
    operand_ids.push_back(scope.AddExpr(*std::move(operand_or)));
  }
  return hir::Expr{
      .type = *type_id,
      .data = hir::ConcatExpr{.operands = std::move(operand_ids)},
      .span = span,
  };
}

auto LowerAssignmentPatternFromElementsStructural(
    ScopeLowerer& scope, WalkFrame frame,
    const slang::ast::AssignmentPatternExpressionBase& ap,
    const slang::ast::Expression& expr, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto type_id = scope.Module().GetTypeIdOf(expr);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  std::vector<hir::ExprId> element_ids;
  element_ids.reserve(ap.elements().size());
  for (const auto* elem : ap.elements()) {
    auto lowered = LowerStructuralExpr(scope, frame, *elem);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    element_ids.push_back(scope.AddExpr(*std::move(lowered)));
  }
  return hir::Expr{
      .type = *type_id,
      .data = hir::AssignmentPatternExpr{.elements = std::move(element_ids)},
      .span = span,
  };
}

auto LowerReplicatedAssignmentPatternExprStructural(
    ScopeLowerer& scope, WalkFrame frame,
    const slang::ast::ReplicatedAssignmentPatternExpression& rp,
    const slang::ast::Expression& expr, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto type_id = scope.Module().GetTypeIdOf(expr);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  auto count_or = LowerStructuralExpr(scope, frame, rp.count());
  if (!count_or) return std::unexpected(std::move(count_or.error()));
  const hir::ExprId count_id = scope.AddExpr(*std::move(count_or));
  std::vector<hir::ExprId> item_ids;
  item_ids.reserve(rp.elements().size());
  for (const auto* elem : rp.elements()) {
    auto lowered = LowerStructuralExpr(scope, frame, *elem);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    item_ids.push_back(scope.AddExpr(*std::move(lowered)));
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

}  // namespace lyra::lowering::ast_to_hir
