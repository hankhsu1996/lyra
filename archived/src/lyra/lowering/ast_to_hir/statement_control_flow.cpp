#include "lyra/lowering/ast_to_hir/statement_control_flow.hpp"

#include <optional>
#include <vector>

#include <slang/ast/expressions/OperatorExpressions.h>

#include "lyra/common/constant.hpp"
#include "lyra/common/constant_arena.hpp"
#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "lyra/common/integral_constant.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/hir/expression.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/hir/operator.hpp"
#include "lyra/hir/statement.hpp"
#include "lyra/lowering/ast_to_hir/detail/statement_lowering.hpp"
#include "lyra/lowering/ast_to_hir/expression.hpp"
#include "lyra/lowering/ast_to_hir/statement.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerConditionalStatement(
    const slang::ast::ConditionalStatement& cond_stmt, ScopeLowerer& lowerer)
    -> std::optional<hir::StatementId> {
  StatementLoweringEnv env(lowerer, cond_stmt.sourceRange);

  if (cond_stmt.conditions.size() != 1 ||
      cond_stmt.conditions[0].pattern != nullptr) {
    env.ctx->sink->Error(env.span, "only simple if conditions are supported");
    return hir::kInvalidStatementId;
  }

  hir::UniquePriorityCheck hir_check = hir::UniquePriorityCheck::kNone;
  switch (cond_stmt.check) {
    case slang::ast::UniquePriorityCheck::None:
      hir_check = hir::UniquePriorityCheck::kNone;
      break;
    case slang::ast::UniquePriorityCheck::Unique:
      hir_check = hir::UniquePriorityCheck::kUnique;
      break;
    case slang::ast::UniquePriorityCheck::Unique0:
      hir_check = hir::UniquePriorityCheck::kUnique0;
      break;
    case slang::ast::UniquePriorityCheck::Priority:
      hir_check = hir::UniquePriorityCheck::kPriority;
      break;
  }

  hir::ExpressionId condition = env.LowerExpr(*cond_stmt.conditions[0].expr);
  if (!condition) {
    return hir::kInvalidStatementId;
  }

  auto then_result = LowerStatement(cond_stmt.ifTrue, *env.lowerer);
  if (!then_result.has_value()) {
    env.ctx->sink->Error(env.span, "if-true branch cannot be empty");
    return hir::kInvalidStatementId;
  }
  if (!*then_result) {
    return hir::kInvalidStatementId;
  }
  hir::StatementId then_branch = *then_result;

  std::optional<hir::StatementId> else_branch;
  if (cond_stmt.ifFalse != nullptr) {
    auto else_result = LowerStatement(*cond_stmt.ifFalse, *env.lowerer);
    if (!else_result.has_value()) {
      env.ctx->sink->Error(env.span, "if-false branch cannot be empty");
      return hir::kInvalidStatementId;
    }
    if (!*else_result) {
      return hir::kInvalidStatementId;
    }
    else_branch = *else_result;
  }

  return env.ctx->hir_arena->AddStatement(
      hir::Statement{
          .kind = hir::StatementKind::kConditional,
          .span = env.span,
          .data = hir::ConditionalStatementData{
              .condition = condition,
              .then_branch = then_branch,
              .else_branch = else_branch,
              .check = hir_check}});
}

auto LowerCaseStatement(
    const slang::ast::CaseStatement& case_stmt, ScopeLowerer& lowerer)
    -> std::optional<hir::StatementId> {
  StatementLoweringEnv env(lowerer, case_stmt.sourceRange);

  hir::CaseCondition hir_condition = hir::CaseCondition::kNormal;
  switch (case_stmt.condition) {
    case slang::ast::CaseStatementCondition::Normal:
      hir_condition = hir::CaseCondition::kNormal;
      break;
    case slang::ast::CaseStatementCondition::WildcardJustZ:
      hir_condition = hir::CaseCondition::kCaseZ;
      break;
    case slang::ast::CaseStatementCondition::WildcardXOrZ:
      hir_condition = hir::CaseCondition::kCaseX;
      break;
    case slang::ast::CaseStatementCondition::Inside:
      hir_condition = hir::CaseCondition::kInside;
      break;
  }

  hir::UniquePriorityCheck hir_check = hir::UniquePriorityCheck::kNone;
  switch (case_stmt.check) {
    case slang::ast::UniquePriorityCheck::None:
      hir_check = hir::UniquePriorityCheck::kNone;
      break;
    case slang::ast::UniquePriorityCheck::Unique:
      hir_check = hir::UniquePriorityCheck::kUnique;
      break;
    case slang::ast::UniquePriorityCheck::Unique0:
      hir_check = hir::UniquePriorityCheck::kUnique0;
      break;
    case slang::ast::UniquePriorityCheck::Priority:
      hir_check = hir::UniquePriorityCheck::kPriority;
      break;
  }

  hir::ExpressionId selector = env.LowerExpr(case_stmt.expr);
  if (!selector) {
    return hir::kInvalidStatementId;
  }

  TypeId bit_type = env.ctx->LogicType();

  std::vector<hir::CaseItem> items;
  for (const auto& group : case_stmt.items) {
    std::optional<hir::ExpressionId> raw_pred;

    for (const slang::ast::Expression* expr : group.expressions) {
      hir::ExpressionId item_pred;

      if (hir_condition == hir::CaseCondition::kInside) {
        if (expr->kind == slang::ast::ExpressionKind::ValueRange) {
          const auto& range = expr->as<slang::ast::ValueRangeExpression>();

          if (range.rangeKind != slang::ast::ValueRangeKind::Simple) {
            env.ctx->sink->Error(
                env.span, "tolerance ranges not supported in case inside");
            return hir::kInvalidStatementId;
          }

          hir::ExpressionId lo = env.LowerExpr(range.left());
          if (!lo) {
            return hir::kInvalidStatementId;
          }
          hir::ExpressionId hi = env.LowerExpr(range.right());
          if (!hi) {
            return hir::kInvalidStatementId;
          }

          hir::ExpressionId ge = env.ctx->hir_arena->AddExpression(
              hir::Expression{
                  .kind = hir::ExpressionKind::kBinaryOp,
                  .type = bit_type,
                  .span = env.span,
                  .data = hir::BinaryExpressionData{
                      .op = hir::BinaryOp::kGreaterThanEqual,
                      .lhs = selector,
                      .rhs = lo}});
          hir::ExpressionId le = env.ctx->hir_arena->AddExpression(
              hir::Expression{
                  .kind = hir::ExpressionKind::kBinaryOp,
                  .type = bit_type,
                  .span = env.span,
                  .data = hir::BinaryExpressionData{
                      .op = hir::BinaryOp::kLessThanEqual,
                      .lhs = selector,
                      .rhs = hi}});
          item_pred = env.ctx->hir_arena->AddExpression(
              hir::Expression{
                  .kind = hir::ExpressionKind::kBinaryOp,
                  .type = bit_type,
                  .span = env.span,
                  .data = hir::BinaryExpressionData{
                      .op = hir::BinaryOp::kBitwiseAnd, .lhs = ge, .rhs = le}});
        } else {
          hir::ExpressionId item_val = env.LowerExpr(*expr);
          if (!item_val) {
            return hir::kInvalidStatementId;
          }

          bool use_wildcard = InsideItemUsesWildcardEq(*expr);
          hir::BinaryOp op = use_wildcard ? hir::BinaryOp::kWildcardEqual
                                          : hir::BinaryOp::kEqual;

          item_pred = env.ctx->hir_arena->AddExpression(
              hir::Expression{
                  .kind = hir::ExpressionKind::kBinaryOp,
                  .type = bit_type,
                  .span = env.span,
                  .data = hir::BinaryExpressionData{
                      .op = op, .lhs = selector, .rhs = item_val}});
        }
      } else {
        hir::ExpressionId item_val = env.LowerExpr(*expr);
        if (!item_val) {
          return hir::kInvalidStatementId;
        }

        hir::BinaryOp cmp_op = hir::BinaryOp::kEqual;
        switch (hir_condition) {
          case hir::CaseCondition::kNormal:
            cmp_op = hir::BinaryOp::kCaseEqual;
            break;
          case hir::CaseCondition::kCaseZ:
            cmp_op = hir::BinaryOp::kCaseZMatch;
            break;
          case hir::CaseCondition::kCaseX:
            cmp_op = hir::BinaryOp::kCaseXMatch;
            break;
          case hir::CaseCondition::kInside:
            break;
        }

        item_pred = env.ctx->hir_arena->AddExpression(
            hir::Expression{
                .kind = hir::ExpressionKind::kBinaryOp,
                .type = bit_type,
                .span = env.span,
                .data = hir::BinaryExpressionData{
                    .op = cmp_op, .lhs = selector, .rhs = item_val}});
      }

      if (raw_pred) {
        raw_pred = env.ctx->hir_arena->AddExpression(
            hir::Expression{
                .kind = hir::ExpressionKind::kBinaryOp,
                .type = bit_type,
                .span = env.span,
                .data = hir::BinaryExpressionData{
                    .op = hir::BinaryOp::kBitwiseOr,
                    .lhs = *raw_pred,
                    .rhs = item_pred}});
      } else {
        raw_pred = item_pred;
      }
    }

    // Apply 2-state clamp: predicate = (raw_pred === 1'b1)
    hir::ExpressionId predicate;
    if (raw_pred) {
      IntegralConstant one_const;
      one_const.value.push_back(1);
      one_const.unknown.push_back(0);
      ConstId one_id = env.ctx->active_constant_arena->Intern(
          bit_type, std::move(one_const));
      hir::ExpressionId one_expr = env.ctx->hir_arena->AddExpression(
          hir::Expression{
              .kind = hir::ExpressionKind::kConstant,
              .type = bit_type,
              .span = env.span,
              .data = hir::ConstantExpressionData{.constant = one_id}});

      predicate = env.ctx->hir_arena->AddExpression(
          hir::Expression{
              .kind = hir::ExpressionKind::kBinaryOp,
              .type = bit_type,
              .span = env.span,
              .data = hir::BinaryExpressionData{
                  .op = hir::BinaryOp::kCaseEqual,
                  .lhs = *raw_pred,
                  .rhs = one_expr}});
    } else {
      IntegralConstant zero_const;
      zero_const.value.push_back(0);
      zero_const.unknown.push_back(0);
      ConstId zero_id = env.ctx->active_constant_arena->Intern(
          bit_type, std::move(zero_const));
      predicate = env.ctx->hir_arena->AddExpression(
          hir::Expression{
              .kind = hir::ExpressionKind::kConstant,
              .type = bit_type,
              .span = env.span,
              .data = hir::ConstantExpressionData{.constant = zero_id}});
    }

    auto body_result = LowerStatement(*group.stmt, *env.lowerer);
    if (body_result.has_value() && !*body_result) {
      return hir::kInvalidStatementId;
    }
    items.push_back({.predicate = predicate, .statement = body_result});
  }

  std::optional<hir::StatementId> default_statement;
  if (case_stmt.defaultCase != nullptr) {
    auto default_result = LowerStatement(*case_stmt.defaultCase, *env.lowerer);
    if (default_result.has_value()) {
      if (!*default_result) {
        return hir::kInvalidStatementId;
      }
      default_statement = *default_result;
    }
  }

  return env.ctx->hir_arena->AddStatement(
      hir::Statement{
          .kind = hir::StatementKind::kCase,
          .span = env.span,
          .data = hir::CaseStatementData{
              .selector = selector,
              .items = std::move(items),
              .default_statement = default_statement,
              .condition = hir_condition,
              .check = hir_check}});
}

}  // namespace lyra::lowering::ast_to_hir
