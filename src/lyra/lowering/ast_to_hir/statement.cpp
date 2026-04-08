#include "lyra/lowering/ast_to_hir/statement.hpp"

#include <format>
#include <optional>

#include <slang/ast/statements/ConditionalStatements.h>
#include <slang/ast/statements/LoopStatements.h>
#include <slang/ast/statements/MiscStatements.h>

#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "lyra/common/type.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/hir/expression.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/hir/statement.hpp"
#include "lyra/lowering/ast_to_hir/context.hpp"
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

}  // namespace lyra::lowering::ast_to_hir
