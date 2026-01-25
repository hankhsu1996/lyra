#include "lyra/lowering/ast_to_hir/module.hpp"

#include <format>
#include <utility>

#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/MemberSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/VariableSymbols.h>

#include "lyra/hir/arena.hpp"
#include "lyra/hir/expression.hpp"
#include "lyra/hir/routine.hpp"
#include "lyra/hir/statement.hpp"
#include "lyra/lowering/ast_to_hir/context.hpp"
#include "lyra/lowering/ast_to_hir/expression.hpp"
#include "lyra/lowering/ast_to_hir/generate.hpp"
#include "lyra/lowering/ast_to_hir/module_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/routine.hpp"
#include "lyra/lowering/ast_to_hir/source_utils.hpp"
#include "lyra/lowering/ast_to_hir/symbol_registrar.hpp"
#include "lyra/lowering/ast_to_hir/type.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerModule(
    const slang::ast::InstanceSymbol& instance, SymbolRegistrar& registrar,
    Context* ctx) -> hir::Module {
  // Create per-module lowerer (owns timescale state)
  ModuleLowerer lowerer(*ctx, registrar, instance);

  const slang::ast::InstanceBodySymbol& body = instance.body;
  SourceSpan span = ctx->SpanOf(GetSourceRange(instance));

  SymbolId symbol =
      registrar.Register(instance, SymbolKind::kInstance, kInvalidTypeId);

  std::vector<SymbolId> variables;
  std::vector<hir::ProcessId> processes;
  std::vector<hir::FunctionId> functions;
  std::vector<hir::TaskId> tasks;

  // Track variables with initializers for deferred lowering
  // (must wait until function symbols are registered)
  std::vector<std::pair<SymbolId, const slang::ast::Expression*>> var_init_refs;

  {
    ScopeGuard scope_guard(registrar, ScopeKind::kModule);

    CollectedMembers members;
    CollectScopeMembers(body, registrar, members);

    // Phase 1: Register module-level variables (before processes that use them)
    // Store initializer references for deferred lowering
    for (const auto* var : members.variables) {
      TypeId type = LowerType(var->getType(), span, ctx);
      if (type) {
        SymbolId sym = registrar.Register(*var, SymbolKind::kVariable, type);
        variables.push_back(sym);

        // Store reference to initializer (lowered after function registration)
        if (const auto* init = var->getInitializer()) {
          var_init_refs.emplace_back(sym, init);
        }
      }
    }

    // Phase 2: Register function symbols (before processes that call them)
    // Store references to lower their bodies later
    for (const auto* sub : members.functions) {
      const auto& ret_type = sub->getReturnType();
      if (!ret_type.isIntegral() && !ret_type.isVoid()) {
        ctx->sink->Error(span, "only integral or void return types supported");
        continue;
      }
      TypeId return_type = LowerType(ret_type, span, ctx);
      if (!return_type) {
        continue;
      }
      registrar.Register(*sub, SymbolKind::kFunction, return_type);
    }

    // Phase 3: Lower processes (can now reference function symbols)
    for (const auto* proc : members.processes) {
      hir::ProcessId id = LowerProcess(*proc, lowerer);
      if (id) {
        processes.push_back(id);
      }
    }

    // Phase 3b: Desugar continuous assignments into synthetic always_comb
    // Lyra variable-only model; multiple writers use last-writer-wins;
    // no resolution.
    for (const auto* ca : members.continuous_assigns) {
      SourceSpan ca_span = ctx->SpanOf(GetSourceRange(*ca));

      if (const auto* delay = ca->getDelay()) {
        ctx->sink->Error(
            ctx->SpanOf(delay->sourceRange),
            "continuous assignment delays not supported");
        continue;
      }

      const auto& assign_expr = ca->getAssignment();
      const auto& assign = assign_expr.as<slang::ast::AssignmentExpression>();

      hir::ExpressionId target = LowerExpression(assign.left(), registrar, ctx);
      if (!target) {
        continue;
      }

      const auto& target_data = (*ctx->hir_arena)[target];
      if (!hir::IsPlaceExpressionKind(target_data.kind)) {
        ctx->sink->Error(
            target_data.span,
            "continuous assignment target is not a simple place expression");
        continue;
      }

      hir::ExpressionId value = LowerExpression(assign.right(), registrar, ctx);
      if (!value) {
        continue;
      }

      hir::StatementId stmt = ctx->hir_arena->AddStatement(
          hir::Statement{
              .kind = hir::StatementKind::kAssignment,
              .span = ca_span,
              .data =
                  hir::AssignmentStatementData{
                      .target = target,
                      .value = value,
                  },
          });

      hir::ProcessId proc = ctx->hir_arena->AddProcess(
          hir::Process{
              .kind = hir::ProcessKind::kAlwaysComb,
              .span = ca_span,
              .body = stmt,
          });
      processes.push_back(proc);
    }

    // Port bindings are now handled at HIR->MIR level via ApplyBindings

    // Phase 4: Lower function bodies
    for (const auto* sub : members.functions) {
      const auto& ret_type = sub->getReturnType();
      if (!ret_type.isIntegral() && !ret_type.isVoid()) {
        continue;
      }
      hir::FunctionId id = LowerFunction(*sub, lowerer);
      if (id) {
        functions.push_back(id);
      }
    }

    // Phase 5: Lower tasks
    for (const auto* sub : members.tasks) {
      hir::TaskId id = LowerTask(*sub, lowerer);
      if (id) {
        tasks.push_back(id);
      }
    }

    // Phase 6: Generate synthetic init process for module variable initializers
    // (after function symbols are registered so initializers can call
    // functions)
    if (!var_init_refs.empty()) {
      std::vector<hir::StatementId> init_stmts;
      init_stmts.reserve(var_init_refs.size());

      for (const auto& [sym, init_ast] : var_init_refs) {
        // Lower the initializer expression (now that functions are registered)
        hir::ExpressionId init_expr =
            LowerExpression(*init_ast, registrar, ctx);
        if (!init_expr) {
          continue;
        }

        // Create NameRef for the variable target
        TypeId var_type = (*ctx->symbol_table)[sym].type;
        hir::ExpressionId target_expr = ctx->hir_arena->AddExpression(
            hir::Expression{
                .kind = hir::ExpressionKind::kNameRef,
                .type = var_type,
                .span = span,
                .data = hir::NameRefExpressionData{.symbol = sym},
            });

        // Create assignment statement: var = init_expr
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

      // Wrap in block statement if we have any valid initializers
      if (!init_stmts.empty()) {
        hir::StatementId body = ctx->hir_arena->AddStatement(
            hir::Statement{
                .kind = hir::StatementKind::kBlock,
                .span = span,
                .data =
                    hir::BlockStatementData{
                        .statements = std::move(init_stmts)},
            });

        // Create synthetic init process
        hir::ProcessId init_proc = ctx->hir_arena->AddProcess(
            hir::Process{
                .kind = hir::ProcessKind::kInitial,
                .span = span,
                .body = body,
            });

        // Insert at front so it runs before user-defined initial blocks
        processes.insert(processes.begin(), init_proc);
      }
    }
  }

  return hir::Module{
      .symbol = symbol,
      .span = span,
      .variables = std::move(variables),
      .processes = std::move(processes),
      .functions = std::move(functions),
      .tasks = std::move(tasks),
  };
}

}  // namespace lyra::lowering::ast_to_hir
