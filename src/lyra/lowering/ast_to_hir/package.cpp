#include "lyra/lowering/ast_to_hir/package.hpp"

#include <utility>

#include <slang/ast/symbols/VariableSymbols.h>

#include "lyra/hir/expression.hpp"
#include "lyra/hir/routine.hpp"
#include "lyra/hir/statement.hpp"
#include "lyra/lowering/ast_to_hir/context.hpp"
#include "lyra/lowering/ast_to_hir/expression.hpp"
#include "lyra/lowering/ast_to_hir/source_utils.hpp"
#include "lyra/lowering/ast_to_hir/symbol_registrar.hpp"
#include "lyra/lowering/ast_to_hir/type.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerPackage(
    const slang::ast::PackageSymbol& package, SymbolRegistrar& registrar,
    Context* ctx) -> hir::Package {
  SourceSpan span = ctx->SpanOf(GetSourceRange(package));

  SymbolId symbol =
      registrar.Register(package, SymbolKind::kPackage, kInvalidTypeId);

  std::vector<SymbolId> variables;
  std::vector<std::pair<SymbolId, const slang::ast::Expression*>> var_init_refs;

  for (const slang::ast::VariableSymbol& var :
       package.membersOfType<slang::ast::VariableSymbol>()) {
    TypeId type = LowerType(var.getType(), span, ctx);
    if (type) {
      SymbolId sym = registrar.Register(var, SymbolKind::kVariable, type);
      variables.push_back(sym);

      if (const auto* init = var.getInitializer()) {
        var_init_refs.emplace_back(sym, init);
      }
    }
  }

  hir::ProcessId init_process = hir::kInvalidProcessId;

  if (!var_init_refs.empty()) {
    std::vector<hir::StatementId> init_stmts;
    init_stmts.reserve(var_init_refs.size());

    for (const auto& [sym, init_ast] : var_init_refs) {
      hir::ExpressionId init_expr = LowerExpression(*init_ast, registrar, ctx);
      if (!init_expr) {
        continue;
      }

      TypeId var_type = (*ctx->symbol_table)[sym].type;
      hir::ExpressionId target_expr = ctx->hir_arena->AddExpression(
          hir::Expression{
              .kind = hir::ExpressionKind::kNameRef,
              .type = var_type,
              .span = span,
              .data = hir::NameRefExpressionData{.symbol = sym},
          });

      hir::StatementId stmt = ctx->hir_arena->AddStatement(
          hir::Statement{
              .kind = hir::StatementKind::kAssignment,
              .span = span,
              .data =
                  hir::AssignmentStatementData{
                      .target = target_expr,
                      .value = init_expr,
                  },
          });
      init_stmts.push_back(stmt);
    }

    if (!init_stmts.empty()) {
      hir::StatementId body = ctx->hir_arena->AddStatement(
          hir::Statement{
              .kind = hir::StatementKind::kBlock,
              .span = span,
              .data =
                  hir::BlockStatementData{.statements = std::move(init_stmts)},
          });

      init_process = ctx->hir_arena->AddProcess(
          hir::Process{
              .kind = hir::ProcessKind::kInitial,
              .span = span,
              .body = body,
          });
    }
  }

  return hir::Package{
      .symbol = symbol,
      .span = span,
      .variables = std::move(variables),
      .init_process = init_process,
  };
}

}  // namespace lyra::lowering::ast_to_hir
