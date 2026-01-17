#include "lyra/lowering/ast_to_hir/statement.hpp"

#include <format>

#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/expressions/CallExpression.h>
#include <slang/ast/expressions/LiteralExpressions.h>
#include <slang/ast/statements/ConditionalStatements.h>
#include <slang/ast/statements/LoopStatements.h>
#include <slang/ast/statements/MiscStatements.h>
#include <slang/ast/symbols/VariableSymbols.h>

#include "lyra/common/constant.hpp"
#include "lyra/common/constant_arena.hpp"
#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "lyra/common/system_function.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/hir/expression.hpp"
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

      ScopeGuard scope_guard(registrar, ScopeKind::kBlock);

      std::vector<hir::StatementId> children;
      children.reserve(list.list.size());
      for (const slang::ast::Statement* child : list.list) {
        auto result = LowerStatement(*child, registrar, ctx);
        if (!result.has_value()) {
          continue;  // Empty statement, skip
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

      // Check if this is a termination system call ($finish, $stop, $exit)
      if (expr_stmt.expr.kind == slang::ast::ExpressionKind::Call) {
        const auto& call = expr_stmt.expr.as<slang::ast::CallExpression>();
        if (call.isSystemCall()) {
          std::string_view name = call.getSubroutineName();
          const SystemFunctionInfo* info = FindSystemFunction(name);
          if (info != nullptr) {
            if (const auto* term_info =
                    std::get_if<TerminationFunctionInfo>(&info->payload)) {
              // Parse the level argument (default if not provided)
              int level = term_info->default_level;
              if (!call.arguments().empty()) {
                const slang::ast::Expression& arg = *call.arguments()[0];
                if (arg.kind == slang::ast::ExpressionKind::IntegerLiteral) {
                  const auto& literal = arg.as<slang::ast::IntegerLiteral>();
                  auto val = literal.getValue();
                  if (val.hasUnknown()) {
                    ctx->sink->Error(
                        span, "termination level cannot contain X or Z");
                    return hir::kInvalidStatementId;
                  }
                  level = static_cast<int>(val.as<int>().value());
                } else {
                  ctx->sink->Error(
                      span, "termination level must be a constant integer");
                  return hir::kInvalidStatementId;
                }
              }

              // Map TerminationType to HIR TerminationKind
              hir::TerminationKind kind = hir::TerminationKind::kFinish;
              switch (term_info->type) {
                case TerminationType::kFinish:
                  kind = hir::TerminationKind::kFinish;
                  break;
                case TerminationType::kStop:
                  kind = hir::TerminationKind::kStop;
                  break;
                case TerminationType::kExit:
                  kind = hir::TerminationKind::kExit;
                  break;
              }

              return ctx->hir_arena->AddStatement(
                  hir::Statement{
                      .kind = hir::StatementKind::kTerminate,
                      .span = span,
                      .data =
                          hir::TerminateStatementData{
                              .kind = kind, .level = level},
                  });
            }
          }
        }
      }

      // Not a termination call - lower as regular expression statement
      hir::ExpressionId expr = LowerExpression(expr_stmt.expr, registrar, ctx);
      if (!expr) {
        return hir::kInvalidStatementId;
      }
      return ctx->hir_arena->AddStatement(
          hir::Statement{
              .kind = hir::StatementKind::kExpression,
              .span = span,
              .data = hir::ExpressionStatementData{.expression = expr},
          });
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

    case StatementKind::ForLoop: {
      const auto& for_stmt = stmt.as<slang::ast::ForLoopStatement>();
      SourceSpan span = ctx->SpanOf(stmt.sourceRange);

      // RAII guard ensures PopScope on all exit paths
      ScopeGuard scope_guard(registrar, ScopeKind::kBlock);

      std::vector<hir::StatementId> var_decls;
      std::vector<hir::ExpressionId> init_exprs;

      if (!for_stmt.loopVars.empty()) {
        // Loop-declared variables: for (int i = 0; ...)
        for (const slang::ast::VariableSymbol* var_sym : for_stmt.loopVars) {
          TypeId type = LowerType(var_sym->getType(), span, ctx);
          if (!type) {
            return hir::kInvalidStatementId;
          }

          SymbolId sym =
              registrar.Register(*var_sym, SymbolKind::kVariable, type);

          hir::ExpressionId init = hir::kInvalidExpressionId;
          if (const slang::ast::Expression* init_expr =
                  var_sym->getInitializer()) {
            init = LowerExpression(*init_expr, registrar, ctx);
            if (!init) {
              return hir::kInvalidStatementId;
            }
          }

          hir::StatementId var_decl = ctx->hir_arena->AddStatement(
              hir::Statement{
                  .kind = hir::StatementKind::kVariableDeclaration,
                  .span = span,
                  .data =
                      hir::VariableDeclarationStatementData{
                          .symbol = sym, .init = init},
              });
          var_decls.push_back(var_decl);
        }
      } else {
        // Expression initializers: for (i = 0, j = 3; ...)
        for (const slang::ast::Expression* init_expr : for_stmt.initializers) {
          hir::ExpressionId expr = LowerExpression(*init_expr, registrar, ctx);
          if (!expr) {
            return hir::kInvalidStatementId;
          }
          init_exprs.push_back(expr);
        }
      }

      std::optional<hir::ExpressionId> condition;
      if (for_stmt.stopExpr != nullptr) {
        hir::ExpressionId cond =
            LowerExpression(*for_stmt.stopExpr, registrar, ctx);
        if (!cond) {
          return hir::kInvalidStatementId;
        }
        condition = cond;
      }

      std::vector<hir::ExpressionId> steps;
      for (const slang::ast::Expression* step_expr : for_stmt.steps) {
        hir::ExpressionId expr = LowerExpression(*step_expr, registrar, ctx);
        if (!expr) {
          return hir::kInvalidStatementId;
        }
        steps.push_back(expr);
      }

      auto body_result = LowerStatement(for_stmt.body, registrar, ctx);
      if (!body_result.has_value()) {
        ctx->sink->Error(span, "for loop body cannot be empty");
        return hir::kInvalidStatementId;
      }
      if (!*body_result) {
        return hir::kInvalidStatementId;
      }

      return ctx->hir_arena->AddStatement(
          hir::Statement{
              .kind = hir::StatementKind::kForLoop,
              .span = span,
              .data =
                  hir::ForLoopStatementData{
                      .var_decls = std::move(var_decls),
                      .init_exprs = std::move(init_exprs),
                      .condition = condition,
                      .steps = std::move(steps),
                      .body = *body_result,
                  },
          });
    }

    case StatementKind::WhileLoop: {
      const auto& while_stmt = stmt.as<slang::ast::WhileLoopStatement>();
      SourceSpan span = ctx->SpanOf(stmt.sourceRange);

      hir::ExpressionId condition =
          LowerExpression(while_stmt.cond, registrar, ctx);
      if (!condition) {
        return hir::kInvalidStatementId;
      }

      auto body_result = LowerStatement(while_stmt.body, registrar, ctx);
      if (!body_result.has_value()) {
        ctx->sink->Error(span, "while loop body cannot be empty");
        return hir::kInvalidStatementId;
      }
      if (!*body_result) {
        return hir::kInvalidStatementId;
      }

      return ctx->hir_arena->AddStatement(
          hir::Statement{
              .kind = hir::StatementKind::kWhileLoop,
              .span = span,
              .data =
                  hir::WhileLoopStatementData{
                      .condition = condition, .body = *body_result},
          });
    }

    case StatementKind::DoWhileLoop: {
      const auto& dowhile_stmt = stmt.as<slang::ast::DoWhileLoopStatement>();
      SourceSpan span = ctx->SpanOf(stmt.sourceRange);

      hir::ExpressionId condition =
          LowerExpression(dowhile_stmt.cond, registrar, ctx);
      if (!condition) {
        return hir::kInvalidStatementId;
      }

      auto body_result = LowerStatement(dowhile_stmt.body, registrar, ctx);
      if (!body_result.has_value()) {
        ctx->sink->Error(span, "do-while loop body cannot be empty");
        return hir::kInvalidStatementId;
      }
      if (!*body_result) {
        return hir::kInvalidStatementId;
      }

      return ctx->hir_arena->AddStatement(
          hir::Statement{
              .kind = hir::StatementKind::kDoWhileLoop,
              .span = span,
              .data =
                  hir::DoWhileLoopStatementData{
                      .condition = condition, .body = *body_result},
          });
    }

    case StatementKind::ForeverLoop: {
      const auto& forever_stmt = stmt.as<slang::ast::ForeverLoopStatement>();
      SourceSpan span = ctx->SpanOf(stmt.sourceRange);

      // Create constant "1" expression for while(1)
      // Use 1-bit logic type (standard for boolean conditions)
      TypeId bit_type = ctx->type_arena->Intern(
          TypeKind::kIntegral,
          IntegralInfo{
              .bit_width = 1, .is_signed = false, .is_four_state = true});
      IntegralConstant one_const;
      one_const.value.push_back(1);
      ConstId const_id =
          ctx->constant_arena->Intern(bit_type, std::move(one_const));
      hir::ExpressionId true_expr = ctx->hir_arena->AddExpression(
          hir::Expression{
              .kind = hir::ExpressionKind::kConstant,
              .type = bit_type,
              .span = span,
              .data = hir::ConstantExpressionData{.constant = const_id},
          });

      auto body_result = LowerStatement(forever_stmt.body, registrar, ctx);
      if (!body_result.has_value()) {
        ctx->sink->Error(span, "forever loop body cannot be empty");
        return hir::kInvalidStatementId;
      }
      if (!*body_result) {
        return hir::kInvalidStatementId;
      }

      return ctx->hir_arena->AddStatement(
          hir::Statement{
              .kind = hir::StatementKind::kWhileLoop,
              .span = span,
              .data =
                  hir::WhileLoopStatementData{
                      .condition = true_expr, .body = *body_result},
          });
    }

    case StatementKind::RepeatLoop: {
      const auto& repeat_stmt = stmt.as<slang::ast::RepeatLoopStatement>();
      SourceSpan span = ctx->SpanOf(stmt.sourceRange);

      hir::ExpressionId count =
          LowerExpression(repeat_stmt.count, registrar, ctx);
      if (!count) {
        return hir::kInvalidStatementId;
      }

      auto body_result = LowerStatement(repeat_stmt.body, registrar, ctx);
      if (!body_result.has_value()) {
        ctx->sink->Error(span, "repeat loop body cannot be empty");
        return hir::kInvalidStatementId;
      }
      if (!*body_result) {
        return hir::kInvalidStatementId;
      }

      return ctx->hir_arena->AddStatement(
          hir::Statement{
              .kind = hir::StatementKind::kRepeatLoop,
              .span = span,
              .data =
                  hir::RepeatLoopStatementData{
                      .count = count, .body = *body_result},
          });
    }

    case StatementKind::Break: {
      return ctx->hir_arena->AddStatement(
          hir::Statement{
              .kind = hir::StatementKind::kBreak,
              .span = ctx->SpanOf(stmt.sourceRange),
              .data = hir::BreakStatementData{},
          });
    }

    case StatementKind::Continue: {
      return ctx->hir_arena->AddStatement(
          hir::Statement{
              .kind = hir::StatementKind::kContinue,
              .span = ctx->SpanOf(stmt.sourceRange),
              .data = hir::ContinueStatementData{},
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
