#include "lyra/lowering/ast_to_hir/package.hpp"

#include <utility>
#include <vector>

#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/VariableSymbols.h>

#include "lyra/common/source_span.hpp"
#include "lyra/common/symbol_types.hpp"
#include "lyra/common/type.hpp"
#include "lyra/hir/expression.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/hir/package.hpp"
#include "lyra/hir/routine.hpp"
#include "lyra/hir/statement.hpp"
#include "lyra/lowering/ast_to_hir/context.hpp"
#include "lyra/lowering/ast_to_hir/expression.hpp"
#include "lyra/lowering/ast_to_hir/routine.hpp"
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
  std::vector<const slang::ast::SubroutineSymbol*> function_refs;

  // Phase 1: Register variables and function symbols
  namespace sk = slang::ast;
  for (const auto& member : package.members()) {
    switch (member.kind) {
      case sk::SymbolKind::Variable: {
        const auto& var = member.as<slang::ast::VariableSymbol>();
        TypeId type = LowerType(var.getType(), span, ctx);
        if (type) {
          SymbolId sym = registrar.Register(var, SymbolKind::kVariable, type);
          variables.push_back(sym);
          if (const auto* init = var.getInitializer()) {
            var_init_refs.emplace_back(sym, init);
          }
        }
        break;
      }
      case sk::SymbolKind::Subroutine: {
        const auto& sub = member.as<slang::ast::SubroutineSymbol>();
        if (sub.subroutineKind == slang::ast::SubroutineKind::Task) {
          ctx->ErrorFmt(
              span, "package task '{}' not yet supported", member.name);
          break;
        }
        const auto& ret_type = sub.getReturnType();
        if (!ret_type.isIntegral() && !ret_type.isVoid()) {
          ctx->sink->Error(
              span, "only integral or void return types supported");
          break;
        }
        TypeId return_type = LowerType(ret_type, span, ctx);
        if (!return_type) {
          break;
        }
        registrar.Register(sub, SymbolKind::kFunction, return_type);
        function_refs.push_back(&sub);
        break;
      }
      case sk::SymbolKind::Parameter:
      case sk::SymbolKind::TypeParameter:
      case sk::SymbolKind::TransparentMember:
      case sk::SymbolKind::EmptyMember:
      case sk::SymbolKind::ExplicitImport:
      case sk::SymbolKind::WildcardImport:
        break;
      default:
        if (!member.isType()) {
          ctx->ErrorFmt(span, "unsupported package member '{}'", member.name);
        }
        break;
    }
  }

  // Phase 2: Lower function bodies (symbols are registered, recursion works)
  std::vector<hir::FunctionId> functions;
  for (const slang::ast::SubroutineSymbol* sub : function_refs) {
    hir::FunctionId id = LowerFunction(*sub, registrar, ctx);
    if (id) {
      functions.push_back(id);
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
      .functions = std::move(functions),
      .init_process = init_process,
  };
}

}  // namespace lyra::lowering::ast_to_hir
