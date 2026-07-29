#include "lyra/lowering/ast_to_hir/statement/branches.hpp"

#include <expected>
#include <optional>
#include <utility>
#include <vector>

#include <slang/ast/Statement.h>
#include <slang/ast/statements/ConditionalStatements.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/lowering/ast_to_hir/pattern.hpp"

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

auto LowerCaseCondition(slang::ast::CaseStatementCondition condition)
    -> hir::CaseCondition {
  switch (condition) {
    case slang::ast::CaseStatementCondition::Normal:
      return hir::CaseCondition::kNormal;
    case slang::ast::CaseStatementCondition::WildcardJustZ:
      return hir::CaseCondition::kWildcardJustZ;
    case slang::ast::CaseStatementCondition::WildcardXOrZ:
      return hir::CaseCondition::kWildcardXOrZ;
    case slang::ast::CaseStatementCondition::Inside:
      return hir::CaseCondition::kInside;
  }
  throw InternalError(
      "LowerCaseCondition: unknown slang CaseStatementCondition");
}

}  // namespace

auto LowerCaseStmt(
    ProcessLowerer& proc, WalkFrame frame, const slang::ast::CaseStatement& cs,
    diag::SourceSpan span) -> diag::Result<hir::Stmt> {
  const hir::CaseCondition condition_kind = LowerCaseCondition(cs.condition);
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
  auto clauses_or = LowerConditionClauses(proc, frame, cs.conditions, span);
  if (!clauses_or) return std::unexpected(std::move(clauses_or.error()));
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
              .conditions = *std::move(clauses_or),
              .then_stmt = then_id,
              .else_stmt = else_id,
              .check = if_check},
      .span = span};
}

auto LowerPatternCaseStmt(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::PatternCaseStatement& cs, diag::SourceSpan span)
    -> diag::Result<hir::Stmt> {
  const auto case_check = LowerUniquePriorityCheck(cs.check);
  const hir::CaseCondition condition_kind = LowerCaseCondition(cs.condition);
  auto cond_expr = proc.LowerExpr(cs.expr, frame);
  if (!cond_expr) return std::unexpected(std::move(cond_expr.error()));
  const hir::ExprId cond_id = frame.Exprs().Add(*std::move(cond_expr));
  std::vector<hir::PatternCaseItem> items;
  items.reserve(cs.items.size());
  for (const auto& item : cs.items) {
    auto pat_or = AddPattern(proc, frame, *item.pattern, *cs.expr.type, span);
    if (!pat_or) return std::unexpected(std::move(pat_or.error()));
    std::optional<hir::ExprId> filter_id;
    if (item.filter != nullptr) {
      auto filter_or = proc.LowerExpr(*item.filter, frame);
      if (!filter_or) return std::unexpected(std::move(filter_or.error()));
      filter_id = frame.Exprs().Add(*std::move(filter_or));
    }
    auto stmt_or = proc.LowerStmt(*item.stmt, frame);
    if (!stmt_or) return std::unexpected(std::move(stmt_or.error()));
    const hir::StmtId stmt_id =
        frame.current_procedural_body->stmts.Add(*std::move(stmt_or));
    items.push_back(
        hir::PatternCaseItem{
            .pattern = *pat_or, .filter = filter_id, .stmt = stmt_id});
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
          hir::PatternCaseStmt{
              .condition_kind = condition_kind,
              .condition = cond_id,
              .items = std::move(items),
              .default_stmt = default_id,
              .check = case_check},
      .span = span};
}

}  // namespace lyra::lowering::ast_to_hir
