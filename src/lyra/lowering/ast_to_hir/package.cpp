#include "lyra/lowering/ast_to_hir/package.hpp"

#include <format>
#include <utility>
#include <vector>

#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/VariableSymbols.h>

#include "lyra/common/constant_arena.hpp"
#include "lyra/common/source_span.hpp"
#include "lyra/common/symbol_types.hpp"
#include "lyra/common/type.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/hir/expression.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/hir/package.hpp"
#include "lyra/hir/routine.hpp"
#include "lyra/hir/statement.hpp"
#include "lyra/lowering/ast_to_hir/callable_registration.hpp"
#include "lyra/lowering/ast_to_hir/context.hpp"
#include "lyra/lowering/ast_to_hir/expression.hpp"
#include "lyra/lowering/ast_to_hir/module_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/routine.hpp"
#include "lyra/lowering/ast_to_hir/source_utils.hpp"
#include "lyra/lowering/ast_to_hir/symbol_registrar.hpp"
#include "lyra/lowering/ast_to_hir/type.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerPackage(
    const slang::ast::PackageSymbol& package, SymbolRegistrar& registrar,
    Context* ctx) -> hir::Package {
  // Package-local HIR and constant storage. All HIR nodes and constants
  // produced during package lowering are isolated in package-owned arenas.
  hir::Arena package_arena;
  ConstantArena package_constant_arena;
  Context pkg_ctx =
      ctx->ForkForPackageLowering(package_arena, package_constant_arena);

  // Per-package lowerer (owns timescale state). Built against pkg_ctx so all
  // downstream HIR allocations land in package_arena.
  ScopeLowerer lowerer(pkg_ctx, registrar, package);

  SourceSpan span = pkg_ctx.SpanOf(GetSourceRange(package));

  SymbolId symbol =
      registrar.Register(package, SymbolKind::kPackage, kInvalidTypeId);

  std::vector<SymbolId> variables;
  std::vector<std::pair<SymbolId, const slang::ast::Expression*>> var_init_refs;
  std::vector<const slang::ast::SubroutineSymbol*> function_refs;
  std::vector<hir::DpiImportDecl> dpi_imports;

  // Phase 1: Register variables and function symbols
  namespace sk = slang::ast;
  for (const auto& member : package.members()) {
    switch (member.kind) {
      case sk::SymbolKind::Variable: {
        const auto& var = member.as<slang::ast::VariableSymbol>();
        TypeId type = LowerType(var.getType(), span, &pkg_ctx);
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

        // Try DPI import normalization first.
        auto dpi_result = TryLowerDpiImport(sub, registrar, &pkg_ctx);
        switch (dpi_result.kind) {
          case DpiLoweringKind::kAccepted:
            dpi_imports.push_back(std::move(dpi_result.decl).value());
            break;
          case DpiLoweringKind::kRejected:
            break;
          case DpiLoweringKind::kNotDpi: {
            if (sub.subroutineKind == slang::ast::SubroutineKind::Task) {
              pkg_ctx.ErrorFmt(
                  span, "package task '{}' not yet supported", member.name);
              break;
            }
            SourceSpan func_span = pkg_ctx.SpanOf(GetSourceRange(sub));
            SymbolId func_sym = RegisterCallableSymbol(
                sub, SymbolKind::kFunction, pkg_ctx, registrar, func_span);
            if (!func_sym) {
              break;
            }
            function_refs.push_back(&sub);
            break;
          }
        }
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
          pkg_ctx.ErrorFmt(
              span, "unsupported package member '{}'", member.name);
        }
        break;
    }
  }

  // Phase 2: Lower function bodies (symbols are registered, recursion works).
  // LowerFunction writes into lowerer.Ctx().hir_arena, which is package_arena.
  std::vector<hir::FunctionId> functions;
  for (const slang::ast::SubroutineSymbol* sub : function_refs) {
    hir::FunctionId id = LowerFunction(*sub, lowerer);
    if (id) {
      functions.push_back(id);
    }
  }

  hir::ProcessId init_process = hir::kInvalidProcessId;

  if (!var_init_refs.empty()) {
    std::vector<hir::StatementId> init_stmts;
    init_stmts.reserve(var_init_refs.size());

    for (const auto& [sym, init_ast] : var_init_refs) {
      hir::ExpressionId init_expr =
          LowerScopedExpression(*init_ast, pkg_ctx, registrar, lowerer.Frame());
      if (!init_expr) {
        continue;
      }

      TypeId var_type = (*pkg_ctx.symbol_table)[sym].type;
      hir::ExpressionId target_expr = pkg_ctx.hir_arena->AddExpression(
          hir::Expression{
              .kind = hir::ExpressionKind::kNameRef,
              .type = var_type,
              .span = span,
              .data = hir::NameRefExpressionData{.symbol = sym},
          });

      hir::StatementId stmt = pkg_ctx.hir_arena->AddStatement(
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
      hir::StatementId body = pkg_ctx.hir_arena->AddStatement(
          hir::Statement{
              .kind = hir::StatementKind::kBlock,
              .span = span,
              .data =
                  hir::BlockStatementData{.statements = std::move(init_stmts)},
          });

      init_process = pkg_ctx.hir_arena->AddProcess(
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
      .dpi_imports = std::move(dpi_imports),
      .dpi_exports = {},
      .init_process = init_process,
      .arena = std::move(package_arena),
      .constant_arena = std::move(package_constant_arena),
  };
}

}  // namespace lyra::lowering::ast_to_hir
