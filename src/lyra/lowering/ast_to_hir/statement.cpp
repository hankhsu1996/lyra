#include "lyra/lowering/ast_to_hir/statement.hpp"

#include <format>
#include <optional>

#include <slang/ast/expressions/CallExpression.h>
#include <slang/ast/statements/ConditionalStatements.h>
#include <slang/ast/statements/LoopStatements.h>
#include <slang/ast/statements/MiscStatements.h>
#include <slang/ast/symbols/SubroutineSymbols.h>

#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "lyra/common/type.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/hir/expression.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/hir/statement.hpp"
#include "lyra/lowering/ast_to_hir/context.hpp"
#include "lyra/lowering/ast_to_hir/detail/statement_lowering.hpp"
#include "lyra/lowering/ast_to_hir/expression.hpp"
#include "lyra/lowering/ast_to_hir/module_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/statement_assert.hpp"
#include "lyra/lowering/ast_to_hir/statement_control_flow.hpp"
#include "lyra/lowering/ast_to_hir/statement_foreach.hpp"
#include "lyra/lowering/ast_to_hir/statement_loops.hpp"
#include "lyra/lowering/ast_to_hir/statement_simple.hpp"
#include "lyra/lowering/ast_to_hir/statement_timing.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerStatement(const slang::ast::Statement& stmt, ScopeLowerer& lowerer)
    -> std::optional<hir::StatementId> {
  auto* ctx = &lowerer.Ctx();
  using slang::ast::StatementKind;

  switch (stmt.kind) {
    case StatementKind::Empty:
      return std::nullopt;

    case StatementKind::List:
      return LowerListStatement(
          stmt.as<slang::ast::StatementList>(), stmt, lowerer);

    case StatementKind::Block:
      return LowerStatement(
          stmt.as<slang::ast::BlockStatement>().body, lowerer);

    case StatementKind::VariableDeclaration:
      return LowerVariableDeclarationStatement(
          stmt.as<slang::ast::VariableDeclStatement>(), lowerer);

    case StatementKind::ExpressionStatement:
      return LowerExpressionStatement(
          stmt.as<slang::ast::ExpressionStatement>(), lowerer);

    case StatementKind::Conditional:
      return LowerConditionalStatement(
          stmt.as<slang::ast::ConditionalStatement>(), lowerer);

    case StatementKind::Case:
      return LowerCaseStatement(stmt.as<slang::ast::CaseStatement>(), lowerer);

    case StatementKind::ForLoop:
      return LowerForLoopStatement(
          stmt.as<slang::ast::ForLoopStatement>(), lowerer);

    case StatementKind::WhileLoop:
      return LowerWhileLoopStatement(
          stmt.as<slang::ast::WhileLoopStatement>(), lowerer);

    case StatementKind::DoWhileLoop:
      return LowerDoWhileLoopStatement(
          stmt.as<slang::ast::DoWhileLoopStatement>(), lowerer);

    case StatementKind::ForeverLoop:
      return LowerForeverLoopStatement(
          stmt.as<slang::ast::ForeverLoopStatement>(), lowerer);

    case StatementKind::RepeatLoop:
      return LowerRepeatLoopStatement(
          stmt.as<slang::ast::RepeatLoopStatement>(), lowerer);

    case StatementKind::Break:
      return ctx->hir_arena->AddStatement(
          hir::Statement{
              .kind = hir::StatementKind::kBreak,
              .span = ctx->SpanOf(stmt.sourceRange),
              .data = hir::BreakStatementData{},
          });

    case StatementKind::Continue:
      return ctx->hir_arena->AddStatement(
          hir::Statement{
              .kind = hir::StatementKind::kContinue,
              .span = ctx->SpanOf(stmt.sourceRange),
              .data = hir::ContinueStatementData{},
          });

    case StatementKind::ForeachLoop:
      return LowerForeachStatement(
          stmt.as<slang::ast::ForeachLoopStatement>(), lowerer);

    case StatementKind::Return:
      return LowerReturnStatement(
          stmt.as<slang::ast::ReturnStatement>(), lowerer);

    case StatementKind::Timed:
      return LowerTimedStatement(
          stmt.as<slang::ast::TimedStatement>(), lowerer);

    case StatementKind::ImmediateAssertion: {
      if (ctx->Options().disable_assertions) {
        return std::nullopt;
      }
      return LowerImmediateAssertionStatement(
          stmt.as<slang::ast::ImmediateAssertionStatement>(), lowerer);
    }

    case StatementKind::ConcurrentAssertion: {
      if (ctx->Options().disable_assertions) {
        return std::nullopt;
      }
      ctx->sink->Error(
          ctx->SpanOf(stmt.sourceRange),
          "concurrent assertion statements are unsupported; "
          "pass --disable-assertions to skip");
      return hir::kInvalidStatementId;
    }

    case StatementKind::EventTrigger: {
      const auto& trigger_stmt = stmt.as<slang::ast::EventTriggerStatement>();
      SourceSpan span = ctx->SpanOf(stmt.sourceRange);

      if (trigger_stmt.isNonBlocking) {
        ctx->sink->Error(
            span, "nonblocking event trigger (->>) not yet supported");
        return hir::kInvalidStatementId;
      }

      auto target = LowerScopedExpression(
          trigger_stmt.target, *ctx, lowerer.Registrar(), lowerer.Frame());
      if (!target) {
        return hir::kInvalidStatementId;
      }

      TypeId target_type = (*ctx->hir_arena)[target].type;
      if ((*ctx->type_arena)[target_type].Kind() != TypeKind::kEvent) {
        ctx->sink->Error(span, "event trigger target must be an event type");
        return hir::kInvalidStatementId;
      }

      return ctx->hir_arena->AddStatement(
          hir::Statement{
              .kind = hir::StatementKind::kEventTrigger,
              .span = span,
              .data = hir::EventTriggerStatementData{.target = target},
          });
    }

    default:
      ctx->sink->Error(
          ctx->SpanOf(stmt.sourceRange),
          std::format("unsupported statement kind '{}'", toString(stmt.kind)));
      return hir::kInvalidStatementId;
  }
}

namespace {

// Extract the call ExpressionId from a lowered action StatementId.
// Returns kInvalidExpressionId if the statement shape doesn't match.
auto ExtractCallExprIdFromLoweredAction(
    hir::StatementId action_id, const hir::Arena& arena) -> hir::ExpressionId {
  const auto& stmt = arena[action_id];
  const hir::Statement* inner = &stmt;

  if (const auto* block = std::get_if<hir::BlockStatementData>(&stmt.data)) {
    if (block->statements.size() != 1) return hir::kInvalidExpressionId;
    inner = &arena[block->statements[0]];
  }

  const auto* expr_stmt =
      std::get_if<hir::ExpressionStatementData>(&inner->data);
  if (expr_stmt == nullptr) return hir::kInvalidExpressionId;

  const auto& expr = arena[expr_stmt->expression];
  if (std::get_if<hir::CallExpressionData>(&expr.data) == nullptr) {
    return hir::kInvalidExpressionId;
  }
  return expr_stmt->expression;
}

}  // namespace

auto LowerAndNormalizeActionBranch(
    const slang::ast::Statement* slang_action, ScopeLowerer& lowerer,
    const hir::Arena& arena, SourceSpan span, const char* branch_label)
    -> LoweredActionBranch {
  // Step 1: absent branch.
  if (slang_action == nullptr) {
    return {.kind = LoweredActionBranchKind::kAbsent};
  }

  // Step 2: lower the child statement.
  auto result = LowerStatement(*slang_action, lowerer);
  if (!result.has_value()) {
    // Branch is present but failed to lower -- invalid, not absent.
    return {.kind = LoweredActionBranchKind::kInvalid};
  }
  if (!*result) {
    return {.kind = LoweredActionBranchKind::kInvalid};
  }
  hir::StatementId stmt_id = *result;

  // Step 3: validate slang action shape -- must be a single user call.
  const slang::ast::Statement* inner = slang_action;
  if (inner->kind == slang::ast::StatementKind::Block) {
    const auto& block = inner->as<slang::ast::BlockStatement>();
    if (block.body.kind == slang::ast::StatementKind::List) {
      const auto& list = block.body.as<slang::ast::StatementList>();
      if (list.list.size() != 1) {
        lowerer.Ctx().sink->Unsupported(
            span,
            std::format(
                "deferred assertion {} must be a single subroutine call",
                branch_label),
            UnsupportedCategory::kFeature);
        return {
            .kind = LoweredActionBranchKind::kInvalid, .statement_id = stmt_id};
      }
      inner = list.list[0];
    } else {
      inner = &block.body;
    }
  }

  if (inner->kind != slang::ast::StatementKind::ExpressionStatement) {
    lowerer.Ctx().sink->Unsupported(
        span,
        std::format(
            "deferred assertion {} must be a single subroutine call",
            branch_label),
        UnsupportedCategory::kFeature);
    return {.kind = LoweredActionBranchKind::kInvalid, .statement_id = stmt_id};
  }

  const auto& expr_stmt = inner->as<slang::ast::ExpressionStatement>();
  if (expr_stmt.expr.kind != slang::ast::ExpressionKind::Call) {
    lowerer.Ctx().sink->Unsupported(
        span,
        std::format(
            "deferred assertion {} must be a single subroutine call",
            branch_label),
        UnsupportedCategory::kFeature);
    return {.kind = LoweredActionBranchKind::kInvalid, .statement_id = stmt_id};
  }

  const auto& call = expr_stmt.expr.as<slang::ast::CallExpression>();
  if (std::get_if<const slang::ast::SubroutineSymbol*>(&call.subroutine) ==
      nullptr) {
    lowerer.Ctx().sink->Unsupported(
        span,
        std::format(
            "deferred assertion {} system task action not yet supported",
            branch_label),
        UnsupportedCategory::kFeature);
    return {.kind = LoweredActionBranchKind::kInvalid, .statement_id = stmt_id};
  }

  // Step 4: extract HIR call expression ID (not pointer -- the arena
  // may reallocate when subsequent branches are lowered).
  auto call_expr_id = ExtractCallExprIdFromLoweredAction(stmt_id, arena);
  if (!call_expr_id) {
    throw common::InternalError(
        "LowerAndNormalizeActionBranch",
        std::format(
            "slang action is a call but HIR did not produce "
            "CallExpressionData for {} branch",
            branch_label));
  }

  return {
      .kind = LoweredActionBranchKind::kValidSingleCall,
      .statement_id = stmt_id,
      .call_expr_id = call_expr_id};
}

}  // namespace lyra::lowering::ast_to_hir
