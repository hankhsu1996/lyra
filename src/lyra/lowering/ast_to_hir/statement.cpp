#include "lyra/lowering/ast_to_hir/statement.hpp"

#include <format>

#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/statements/ConditionalStatements.h>
#include <slang/ast/statements/MiscStatements.h>
#include <slang/ast/symbols/VariableSymbols.h>

#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/hir/statement.hpp"
#include "lyra/lowering/ast_to_hir/context.hpp"
#include "lyra/lowering/ast_to_hir/expression.hpp"
#include "lyra/lowering/ast_to_hir/symbol_registrar.hpp"
#include "lyra/lowering/ast_to_hir/type.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerStatement(
    const slang::ast::Statement& stmt, SymbolRegistrar& registrar, Context* ctx)
    -> std::optional<hir::StatementId> {
  using slang::ast::StatementKind;

  switch (stmt.kind) {
    case StatementKind::Empty: {
      return std::nullopt;
    }

    case StatementKind::List: {
      const auto& list = stmt.as<slang::ast::StatementList>();
      SourceSpan span = ctx->SpanOf(stmt.sourceRange);

      registrar.PushScope(ScopeKind::kBlock);

      std::vector<hir::StatementId> children;
      children.reserve(list.list.size());
      for (const slang::ast::Statement* child : list.list) {
        auto result = LowerStatement(*child, registrar, ctx);
        if (!result.has_value()) {
          continue;
        }
        if (!*result) {
          registrar.PopScope();
          return hir::kInvalidStatementId;
        }
        children.push_back(*result);
      }

      registrar.PopScope();

      return ctx->hir_arena->AddStatement(
          hir::Statement{
              .kind = hir::StatementKind::kBlock,
              .span = span,
              .data =
                  hir::BlockStatementData{.statements = std::move(children)}});
    }

    case StatementKind::Block: {
      const auto& block = stmt.as<slang::ast::BlockStatement>();
      return LowerStatement(block.body, registrar, ctx);
    }

    case StatementKind::VariableDeclaration: {
      const auto& var_decl = stmt.as<slang::ast::VariableDeclStatement>();
      SourceSpan span = ctx->SpanOf(stmt.sourceRange);
      const slang::ast::VariableSymbol& var_sym = var_decl.symbol;

      TypeId type = LowerType(var_sym.getType(), span, ctx);
      if (!type) {
        return hir::kInvalidStatementId;
      }

      SymbolId sym = registrar.Register(var_sym, SymbolKind::kVariable, type);

      hir::ExpressionId init = hir::kInvalidExpressionId;
      if (const slang::ast::Expression* init_expr = var_sym.getInitializer()) {
        init = LowerExpression(*init_expr, registrar, ctx);
        if (!init) {
          return hir::kInvalidStatementId;
        }
      }

      return ctx->hir_arena->AddStatement(
          hir::Statement{
              .kind = hir::StatementKind::kVariableDeclaration,
              .span = span,
              .data = hir::VariableDeclarationStatementData{
                  .symbol = sym, .init = init}});
    }

    case StatementKind::ExpressionStatement: {
      const auto& expr_stmt = stmt.as<slang::ast::ExpressionStatement>();
      SourceSpan span = ctx->SpanOf(stmt.sourceRange);
      const slang::ast::Expression& expr = expr_stmt.expr;

      if (expr.kind == slang::ast::ExpressionKind::Assignment) {
        const auto& assign = expr.as<slang::ast::AssignmentExpression>();
        hir::ExpressionId target =
            LowerExpression(assign.left(), registrar, ctx);
        if (!target) {
          return hir::kInvalidStatementId;
        }
        hir::ExpressionId value =
            LowerExpression(assign.right(), registrar, ctx);
        if (!value) {
          return hir::kInvalidStatementId;
        }
        return ctx->hir_arena->AddStatement(
            hir::Statement{
                .kind = hir::StatementKind::kAssignment,
                .span = span,
                .data = hir::AssignmentStatementData{
                    .target = target, .value = value}});
      }

      hir::ExpressionId expression = LowerExpression(expr, registrar, ctx);
      if (!expression) {
        return hir::kInvalidStatementId;
      }
      return ctx->hir_arena->AddStatement(
          hir::Statement{
              .kind = hir::StatementKind::kExpression,
              .span = span,
              .data = hir::ExpressionStatementData{.expression = expression}});
    }

    case StatementKind::Conditional: {
      const auto& cond_stmt = stmt.as<slang::ast::ConditionalStatement>();
      SourceSpan span = ctx->SpanOf(stmt.sourceRange);

      if (cond_stmt.conditions.size() != 1 ||
          cond_stmt.conditions[0].pattern != nullptr) {
        ctx->sink->Error(span, "only simple if conditions are supported");
        return hir::kInvalidStatementId;
      }

      hir::ExpressionId condition =
          LowerExpression(*cond_stmt.conditions[0].expr, registrar, ctx);
      if (!condition) {
        return hir::kInvalidStatementId;
      }

      auto then_result = LowerStatement(cond_stmt.ifTrue, registrar, ctx);
      if (!then_result.has_value()) {
        ctx->sink->Error(span, "if-true branch cannot be empty");
        return hir::kInvalidStatementId;
      }
      if (!*then_result) {
        return hir::kInvalidStatementId;
      }
      hir::StatementId then_branch = *then_result;

      std::optional<hir::StatementId> else_branch;
      if (cond_stmt.ifFalse != nullptr) {
        auto else_result = LowerStatement(*cond_stmt.ifFalse, registrar, ctx);
        if (!else_result.has_value()) {
          ctx->sink->Error(span, "if-false branch cannot be empty");
          return hir::kInvalidStatementId;
        }
        if (!*else_result) {
          return hir::kInvalidStatementId;
        }
        else_branch = *else_result;
      }

      return ctx->hir_arena->AddStatement(
          hir::Statement{
              .kind = hir::StatementKind::kConditional,
              .span = span,
              .data = hir::ConditionalStatementData{
                  .condition = condition,
                  .then_branch = then_branch,
                  .else_branch = else_branch}});
    }

    case StatementKind::Case: {
      const auto& case_stmt = stmt.as<slang::ast::CaseStatement>();
      SourceSpan span = ctx->SpanOf(stmt.sourceRange);

      if (case_stmt.condition != slang::ast::CaseStatementCondition::Normal) {
        ctx->sink->Error(
            span,
            "only basic case statements are supported (casex/casez/case "
            "inside not yet)");
        return hir::kInvalidStatementId;
      }
      if (case_stmt.check != slang::ast::UniquePriorityCheck::None) {
        ctx->sink->Error(span, "unique/priority case not yet supported");
        return hir::kInvalidStatementId;
      }

      hir::ExpressionId selector =
          LowerExpression(case_stmt.expr, registrar, ctx);
      if (!selector) {
        return hir::kInvalidStatementId;
      }

      std::vector<hir::CaseItem> items;
      for (const auto& group : case_stmt.items) {
        std::vector<hir::ExpressionId> expressions;
        for (const slang::ast::Expression* expr : group.expressions) {
          hir::ExpressionId e = LowerExpression(*expr, registrar, ctx);
          if (!e) {
            return hir::kInvalidStatementId;
          }
          expressions.push_back(e);
        }
        auto body_result = LowerStatement(*group.stmt, registrar, ctx);
        if (!body_result.has_value()) {
          ctx->sink->Error(span, "case item body cannot be empty");
          return hir::kInvalidStatementId;
        }
        if (!*body_result) {
          return hir::kInvalidStatementId;
        }
        items.push_back(
            {.expressions = std::move(expressions), .statement = *body_result});
      }

      std::optional<hir::StatementId> default_statement;
      if (case_stmt.defaultCase != nullptr) {
        auto default_result =
            LowerStatement(*case_stmt.defaultCase, registrar, ctx);
        if (!default_result.has_value()) {
          ctx->sink->Error(span, "default case body cannot be empty");
          return hir::kInvalidStatementId;
        }
        if (!*default_result) {
          return hir::kInvalidStatementId;
        }
        default_statement = *default_result;
      }

      return ctx->hir_arena->AddStatement(
          hir::Statement{
              .kind = hir::StatementKind::kCase,
              .span = span,
              .data = hir::CaseStatementData{
                  .selector = selector,
                  .items = std::move(items),
                  .default_statement = default_statement}});
    }

    default:
      ctx->sink->Error(
          ctx->SpanOf(stmt.sourceRange),
          std::format("unsupported statement kind '{}'", toString(stmt.kind)));
      return hir::kInvalidStatementId;
  }
}

}  // namespace lyra::lowering::ast_to_hir
