#include "lyra/lowering/ast_to_hir/statement_simple.hpp"

#include <optional>
#include <vector>

#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/AllTypes.h>

#include "lyra/common/scope_types.hpp"
#include "lyra/common/symbol_types.hpp"
#include "lyra/common/type.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/hir/rvalue.hpp"
#include "lyra/hir/statement.hpp"
#include "lyra/lowering/ast_to_hir/detail/expression_lowering.hpp"
#include "lyra/lowering/ast_to_hir/detail/statement_lowering.hpp"
#include "lyra/lowering/ast_to_hir/pattern.hpp"
#include "lyra/lowering/ast_to_hir/statement.hpp"
#include "lyra/lowering/ast_to_hir/statement_termination.hpp"
#include "lyra/lowering/ast_to_hir/type.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerListStatement(
    const slang::ast::StatementList& list, const slang::ast::Statement& stmt,
    ScopeLowerer& lowerer) -> std::optional<hir::StatementId> {
  auto& registrar = lowerer.Registrar();
  auto* ctx = &lowerer.Ctx();
  SourceSpan span = ctx->SpanOf(stmt.sourceRange);

  ScopeGuard scope_guard(registrar, ScopeKind::kBlock);

  std::vector<hir::StatementId> children;
  children.reserve(list.list.size());
  for (const slang::ast::Statement* child : list.list) {
    auto result = LowerStatement(*child, lowerer);
    if (!result.has_value()) {
      continue;
    }
    if (!*result) {
      return hir::kInvalidStatementId;
    }
    children.push_back(*result);
  }

  return ctx->hir_arena->AddStatement(
      hir::Statement{
          .kind = hir::StatementKind::kBlock,
          .span = span,
          .data = hir::BlockStatementData{.statements = std::move(children)}});
}

auto LowerVariableDeclarationStatement(
    const slang::ast::VariableDeclStatement& stmt, ScopeLowerer& lowerer)
    -> std::optional<hir::StatementId> {
  StatementLoweringEnv env(lowerer, stmt.sourceRange);
  const slang::ast::VariableSymbol& var_sym = stmt.symbol;

  TypeId type = LowerType(var_sym.getType(), env.span, env.ctx);
  if (!type) {
    return hir::kInvalidStatementId;
  }

  SymbolId sym = env.registrar->Register(
      var_sym, SymbolKind::kVariable, type, StorageClass::kLocalStorage);

  std::optional<hir::RValue> initializer;
  if (const slang::ast::Expression* init_expr = var_sym.getInitializer()) {
    if (init_expr->kind ==
        slang::ast::ExpressionKind::StructuredAssignmentPattern) {
      const auto& pattern_expr =
          init_expr->as<slang::ast::StructuredAssignmentPatternExpression>();

      const Type& var_type = (*env.ctx->type_arena)[type];
      if (var_type.Kind() == TypeKind::kPackedArray ||
          var_type.Kind() == TypeKind::kIntegral) {
        ExpressionLoweringView view{
            .context = env.ctx, .registrar = env.registrar, .frame = env.frame};
        hir::PatternId pattern_id =
            LowerPattern(pattern_expr, type, env.span, view);

        if (!pattern_id) {
          return hir::kInvalidStatementId;
        }

        initializer = hir::RValue::Pattern(pattern_id);
      } else {
        hir::ExpressionId init_id = env.LowerExpr(*init_expr);
        if (!init_id) {
          return hir::kInvalidStatementId;
        }
        initializer = hir::RValue::Expression(init_id);
      }
    } else {
      hir::ExpressionId init_id = env.LowerExpr(*init_expr);
      if (!init_id) {
        return hir::kInvalidStatementId;
      }
      initializer = hir::RValue::Expression(init_id);
    }
  }

  return env.ctx->hir_arena->AddStatement(
      hir::Statement{
          .kind = hir::StatementKind::kVariableDeclaration,
          .span = env.span,
          .data = hir::VariableDeclarationStatementData{
              .symbol = sym, .initializer = initializer}});
}

auto LowerExpressionStatement(
    const slang::ast::ExpressionStatement& expr_stmt, ScopeLowerer& lowerer)
    -> std::optional<hir::StatementId> {
  auto termination = TryLowerTerminationCall(expr_stmt, lowerer);
  if (termination.has_value()) {
    return *termination;
  }

  StatementLoweringEnv env(lowerer, expr_stmt.sourceRange);
  hir::ExpressionId expr = env.LowerExpr(expr_stmt.expr);
  if (!expr) {
    return hir::kInvalidStatementId;
  }
  return env.ctx->hir_arena->AddStatement(
      hir::Statement{
          .kind = hir::StatementKind::kExpression,
          .span = env.span,
          .data = hir::ExpressionStatementData{.expression = expr},
      });
}

auto LowerReturnStatement(
    const slang::ast::ReturnStatement& ret, ScopeLowerer& lowerer)
    -> std::optional<hir::StatementId> {
  StatementLoweringEnv env(lowerer, ret.sourceRange);

  hir::ExpressionId value = hir::kInvalidExpressionId;
  if (ret.expr != nullptr) {
    value = env.LowerExpr(*ret.expr);
    if (!value) {
      return hir::kInvalidStatementId;
    }
  }

  return env.ctx->hir_arena->AddStatement(
      hir::Statement{
          .kind = hir::StatementKind::kReturn,
          .span = env.span,
          .data = hir::ReturnStatementData{.value = value},
      });
}

}  // namespace lyra::lowering::ast_to_hir
