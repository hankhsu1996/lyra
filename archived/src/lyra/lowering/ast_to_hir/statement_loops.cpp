#include "lyra/lowering/ast_to_hir/statement_loops.hpp"

#include <format>
#include <optional>
#include <string_view>
#include <vector>

#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/AllTypes.h>

#include "lyra/common/constant_arena.hpp"
#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "lyra/common/integral_constant.hpp"
#include "lyra/common/scope_types.hpp"
#include "lyra/common/symbol_types.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/hir/expression.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/hir/rvalue.hpp"
#include "lyra/hir/statement.hpp"
#include "lyra/lowering/ast_to_hir/detail/statement_lowering.hpp"
#include "lyra/lowering/ast_to_hir/statement.hpp"
#include "lyra/lowering/ast_to_hir/type.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

auto LowerRequiredChildStatement(
    const slang::ast::Statement& child, ScopeLowerer& lowerer, SourceSpan span,
    std::string_view what) -> hir::StatementId {
  auto result = LowerStatement(child, lowerer);
  if (!result.has_value()) {
    lowerer.Ctx().sink->Error(span, std::format("{} cannot be empty", what));
    return hir::kInvalidStatementId;
  }
  if (!*result) return hir::kInvalidStatementId;
  return *result;
}

}  // namespace

auto LowerForLoopStatement(
    const slang::ast::ForLoopStatement& for_stmt, ScopeLowerer& lowerer)
    -> std::optional<hir::StatementId> {
  StatementLoweringEnv env(lowerer, for_stmt.sourceRange);

  ScopeGuard scope_guard(*env.registrar, ScopeKind::kBlock);

  std::vector<hir::StatementId> var_decls;
  std::vector<hir::ExpressionId> init_exprs;

  if (!for_stmt.loopVars.empty()) {
    for (const slang::ast::VariableSymbol* var_sym : for_stmt.loopVars) {
      TypeId type = LowerType(var_sym->getType(), env.span, env.ctx);
      if (!type) {
        return hir::kInvalidStatementId;
      }

      SymbolId sym = env.registrar->Register(
          *var_sym, SymbolKind::kVariable, type, StorageClass::kLocalStorage);

      hir::ExpressionId init = hir::kInvalidExpressionId;
      if (const slang::ast::Expression* init_expr = var_sym->getInitializer()) {
        init = env.LowerExpr(*init_expr);
        if (!init) {
          return hir::kInvalidStatementId;
        }
      }

      std::optional<hir::RValue> initializer;
      if (init) {
        initializer = hir::RValue::Expression(init);
      }
      hir::StatementId var_decl = env.ctx->hir_arena->AddStatement(
          hir::Statement{
              .kind = hir::StatementKind::kVariableDeclaration,
              .span = env.span,
              .data =
                  hir::VariableDeclarationStatementData{
                      .symbol = sym, .initializer = initializer},
          });
      var_decls.push_back(var_decl);
    }
  } else {
    for (const slang::ast::Expression* init_expr : for_stmt.initializers) {
      hir::ExpressionId expr = env.LowerExpr(*init_expr);
      if (!expr) {
        return hir::kInvalidStatementId;
      }
      init_exprs.push_back(expr);
    }
  }

  std::optional<hir::ExpressionId> condition;
  if (for_stmt.stopExpr != nullptr) {
    hir::ExpressionId cond = env.LowerExpr(*for_stmt.stopExpr);
    if (!cond) {
      return hir::kInvalidStatementId;
    }
    condition = cond;
  }

  std::vector<hir::ExpressionId> steps;
  for (const slang::ast::Expression* step_expr : for_stmt.steps) {
    hir::ExpressionId expr = env.LowerExpr(*step_expr);
    if (!expr) {
      return hir::kInvalidStatementId;
    }
    steps.push_back(expr);
  }

  hir::StatementId body = LowerRequiredChildStatement(
      for_stmt.body, lowerer, env.span, "for loop body");
  if (!body) {
    return hir::kInvalidStatementId;
  }

  return env.ctx->hir_arena->AddStatement(
      hir::Statement{
          .kind = hir::StatementKind::kForLoop,
          .span = env.span,
          .data =
              hir::ForLoopStatementData{
                  .var_decls = std::move(var_decls),
                  .init_exprs = std::move(init_exprs),
                  .condition = condition,
                  .steps = std::move(steps),
                  .body = body,
              },
      });
}

auto LowerWhileLoopStatement(
    const slang::ast::WhileLoopStatement& stmt, ScopeLowerer& lowerer)
    -> std::optional<hir::StatementId> {
  StatementLoweringEnv env(lowerer, stmt.sourceRange);

  hir::ExpressionId condition = env.LowerExpr(stmt.cond);
  if (!condition) {
    return hir::kInvalidStatementId;
  }

  hir::StatementId body = LowerRequiredChildStatement(
      stmt.body, lowerer, env.span, "while loop body");
  if (!body) {
    return hir::kInvalidStatementId;
  }

  return env.ctx->hir_arena->AddStatement(
      hir::Statement{
          .kind = hir::StatementKind::kWhileLoop,
          .span = env.span,
          .data =
              hir::WhileLoopStatementData{.condition = condition, .body = body},
      });
}

auto LowerDoWhileLoopStatement(
    const slang::ast::DoWhileLoopStatement& stmt, ScopeLowerer& lowerer)
    -> std::optional<hir::StatementId> {
  StatementLoweringEnv env(lowerer, stmt.sourceRange);

  hir::ExpressionId condition = env.LowerExpr(stmt.cond);
  if (!condition) {
    return hir::kInvalidStatementId;
  }

  hir::StatementId body = LowerRequiredChildStatement(
      stmt.body, lowerer, env.span, "do-while loop body");
  if (!body) {
    return hir::kInvalidStatementId;
  }

  return env.ctx->hir_arena->AddStatement(
      hir::Statement{
          .kind = hir::StatementKind::kDoWhileLoop,
          .span = env.span,
          .data =
              hir::DoWhileLoopStatementData{
                  .condition = condition, .body = body},
      });
}

auto LowerForeverLoopStatement(
    const slang::ast::ForeverLoopStatement& stmt, ScopeLowerer& lowerer)
    -> std::optional<hir::StatementId> {
  StatementLoweringEnv env(lowerer, stmt.sourceRange);

  TypeId logic_type = env.ctx->LogicType();
  IntegralConstant one_const;
  one_const.value.push_back(1);
  one_const.unknown.push_back(0);
  ConstId const_id =
      env.ctx->active_constant_arena->Intern(logic_type, std::move(one_const));
  hir::ExpressionId true_expr = env.ctx->hir_arena->AddExpression(
      hir::Expression{
          .kind = hir::ExpressionKind::kConstant,
          .type = logic_type,
          .span = env.span,
          .data = hir::ConstantExpressionData{.constant = const_id},
      });

  hir::StatementId body = LowerRequiredChildStatement(
      stmt.body, lowerer, env.span, "forever loop body");
  if (!body) {
    return hir::kInvalidStatementId;
  }

  return env.ctx->hir_arena->AddStatement(
      hir::Statement{
          .kind = hir::StatementKind::kWhileLoop,
          .span = env.span,
          .data =
              hir::WhileLoopStatementData{.condition = true_expr, .body = body},
      });
}

auto LowerRepeatLoopStatement(
    const slang::ast::RepeatLoopStatement& stmt, ScopeLowerer& lowerer)
    -> std::optional<hir::StatementId> {
  StatementLoweringEnv env(lowerer, stmt.sourceRange);

  hir::ExpressionId count = env.LowerExpr(stmt.count);
  if (!count) {
    return hir::kInvalidStatementId;
  }

  hir::StatementId body = LowerRequiredChildStatement(
      stmt.body, lowerer, env.span, "repeat loop body");
  if (!body) {
    return hir::kInvalidStatementId;
  }

  return env.ctx->hir_arena->AddStatement(
      hir::Statement{
          .kind = hir::StatementKind::kRepeatLoop,
          .span = env.span,
          .data = hir::RepeatLoopStatementData{.count = count, .body = body},
      });
}

}  // namespace lyra::lowering::ast_to_hir
