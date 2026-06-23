#include "lyra/lowering/ast_to_hir/statement/branches.hpp"

#include <expected>
#include <optional>
#include <utility>
#include <vector>

#include <slang/ast/Statement.h>
#include <slang/ast/statements/ConditionalStatements.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/kind.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

auto LowerUniquePriorityCheck(slang::ast::UniquePriorityCheck check)
    -> std::optional<hir::UniquePriorityCheck> {
  switch (check) {
    case slang::ast::UniquePriorityCheck::None:
      return std::nullopt;
    case slang::ast::UniquePriorityCheck::Unique:
      return hir::UniquePriorityCheck::kUnique;
    case slang::ast::UniquePriorityCheck::Unique0:
      return hir::UniquePriorityCheck::kUnique0;
    case slang::ast::UniquePriorityCheck::Priority:
      return hir::UniquePriorityCheck::kPriority;
  }
  throw InternalError(
      "LowerUniquePriorityCheck: unknown slang UniquePriorityCheck value");
}

auto LowerCaseInsideStmt(
    ProcessLowerer& proc, WalkFrame frame, const slang::ast::CaseStatement& cs,
    diag::SourceSpan span) -> diag::Result<hir::Stmt> {
  const auto case_check = LowerUniquePriorityCheck(cs.check);
  auto cond_expr = proc.LowerExpr(cs.expr, frame);
  if (!cond_expr) return std::unexpected(std::move(cond_expr.error()));
  const hir::ExprId cond_id = frame.Exprs().Add(*std::move(cond_expr));
  std::vector<hir::CaseInsideItem> items;
  items.reserve(cs.items.size());
  for (const auto& item : cs.items) {
    std::vector<hir::InsideItem> inside_items;
    inside_items.reserve(item.expressions.size());
    for (const auto* label_expr : item.expressions) {
      auto item_or = proc.LowerInsideItem(*label_expr, frame);
      if (!item_or) return std::unexpected(std::move(item_or.error()));
      inside_items.push_back(*std::move(item_or));
    }
    auto item_stmt = proc.LowerStmt(*item.stmt, frame);
    if (!item_stmt) return std::unexpected(std::move(item_stmt.error()));
    const hir::StmtId item_id =
        frame.current_procedural_body->stmts.Add(*std::move(item_stmt));
    items.push_back(
        hir::CaseInsideItem{.items = std::move(inside_items), .stmt = item_id});
  }
  std::optional<hir::StmtId> default_id;
  if (cs.defaultCase != nullptr) {
    auto default_stmt = proc.LowerStmt(*cs.defaultCase, frame);
    if (!default_stmt) return std::unexpected(std::move(default_stmt.error()));
    default_id =
        frame.current_procedural_body->stmts.Add(*std::move(default_stmt));
  }
  return hir::Stmt{
      .label = std::nullopt,
      .data =
          hir::CaseInsideStmt{
              .condition = cond_id,
              .items = std::move(items),
              .default_stmt = default_id,
              .check = case_check},
      .span = span};
}

}  // namespace

auto LowerCaseStmt(
    ProcessLowerer& proc, WalkFrame frame, const slang::ast::CaseStatement& cs,
    diag::SourceSpan span) -> diag::Result<hir::Stmt> {
  if (cs.condition == slang::ast::CaseStatementCondition::Inside) {
    return LowerCaseInsideStmt(proc, frame, cs, span);
  }
  const hir::CaseCondition condition_kind = [&] {
    switch (cs.condition) {
      case slang::ast::CaseStatementCondition::Normal:
        return hir::CaseCondition::kNormal;
      case slang::ast::CaseStatementCondition::WildcardJustZ:
        return hir::CaseCondition::kWildcardJustZ;
      case slang::ast::CaseStatementCondition::WildcardXOrZ:
        return hir::CaseCondition::kWildcardXOrZ;
      case slang::ast::CaseStatementCondition::Inside:
        break;
    }
    throw InternalError(
        "LowerCaseStmt: Inside should have been dispatched above");
  }();
  const auto case_check = LowerUniquePriorityCheck(cs.check);
  auto cond_expr = proc.LowerExpr(cs.expr, frame);
  if (!cond_expr) return std::unexpected(std::move(cond_expr.error()));
  const hir::ExprId cond_id = frame.Exprs().Add(*std::move(cond_expr));
  std::vector<hir::CaseItem> items;
  items.reserve(cs.items.size());
  for (const auto& item : cs.items) {
    std::vector<hir::ExprId> label_ids;
    label_ids.reserve(item.expressions.size());
    for (const auto* label_expr : item.expressions) {
      auto label_or = proc.LowerExpr(*label_expr, frame);
      if (!label_or) return std::unexpected(std::move(label_or.error()));
      label_ids.push_back(frame.Exprs().Add(*std::move(label_or)));
    }
    auto item_stmt = proc.LowerStmt(*item.stmt, frame);
    if (!item_stmt) return std::unexpected(std::move(item_stmt.error()));
    const hir::StmtId item_id =
        frame.current_procedural_body->stmts.Add(*std::move(item_stmt));
    items.push_back(
        hir::CaseItem{.labels = std::move(label_ids), .stmt = item_id});
  }
  std::optional<hir::StmtId> default_id;
  if (cs.defaultCase != nullptr) {
    auto default_stmt = proc.LowerStmt(*cs.defaultCase, frame);
    if (!default_stmt) return std::unexpected(std::move(default_stmt.error()));
    default_id =
        frame.current_procedural_body->stmts.Add(*std::move(default_stmt));
  }
  return hir::Stmt{
      .label = std::nullopt,
      .data =
          hir::CaseStmt{
              .condition_kind = condition_kind,
              .condition = cond_id,
              .items = std::move(items),
              .default_stmt = default_id,
              .check = case_check},
      .span = span};
}

auto LowerConditionalStmt(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::ConditionalStatement& cs, diag::SourceSpan span)
    -> diag::Result<hir::Stmt> {
  const auto if_check = LowerUniquePriorityCheck(cs.check);
  if (cs.conditions.size() != 1) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedStatementForm,
        "multi-condition if expressions are not yet supported",
        diag::UnsupportedCategory::kFeature);
  }
  const auto& cond = cs.conditions.front();
  if (cond.pattern != nullptr) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedStatementForm,
        "pattern matching in if conditions is not yet supported",
        diag::UnsupportedCategory::kFeature);
  }
  auto cond_expr = proc.LowerExpr(*cond.expr, frame);
  if (!cond_expr) return std::unexpected(std::move(cond_expr.error()));
  const hir::ExprId cond_id = frame.Exprs().Add(*std::move(cond_expr));
  auto then_stmt = proc.LowerStmt(cs.ifTrue, frame);
  if (!then_stmt) return std::unexpected(std::move(then_stmt.error()));
  const hir::StmtId then_id =
      frame.current_procedural_body->stmts.Add(*std::move(then_stmt));
  std::optional<hir::StmtId> else_id;
  if (cs.ifFalse != nullptr) {
    auto else_stmt = proc.LowerStmt(*cs.ifFalse, frame);
    if (!else_stmt) return std::unexpected(std::move(else_stmt.error()));
    else_id = frame.current_procedural_body->stmts.Add(*std::move(else_stmt));
  }
  return hir::Stmt{
      .label = std::nullopt,
      .data =
          hir::IfStmt{
              .condition = cond_id,
              .then_stmt = then_id,
              .else_stmt = else_id,
              .check = if_check},
      .span = span};
}

}  // namespace lyra::lowering::ast_to_hir
