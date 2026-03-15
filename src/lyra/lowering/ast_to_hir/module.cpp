#include "lyra/lowering/ast_to_hir/module.hpp"

#include <format>
#include <string>
#include <utility>
#include <vector>

#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/MemberSymbols.h>
#include <slang/ast/symbols/ParameterSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/ValueSymbol.h>
#include <slang/ast/symbols/VariableSymbols.h>

#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/scope_types.hpp"
#include "lyra/common/source_span.hpp"
#include "lyra/common/symbol.hpp"
#include "lyra/common/type.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/hir/expression.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/hir/module.hpp"
#include "lyra/hir/module_body.hpp"
#include "lyra/hir/routine.hpp"
#include "lyra/hir/statement.hpp"
#include "lyra/lowering/ast_to_hir/constant.hpp"
#include "lyra/lowering/ast_to_hir/context.hpp"
#include "lyra/lowering/ast_to_hir/expression.hpp"
#include "lyra/lowering/ast_to_hir/module_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/routine.hpp"
#include "lyra/lowering/ast_to_hir/source_utils.hpp"
#include "lyra/lowering/ast_to_hir/symbol_registrar.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerModuleBody(
    const slang::ast::InstanceSymbol& representative,
    const BodyLoweringInput& input, SymbolRegistrar& registrar, Context* ctx)
    -> BodyLoweringResult {
  // Body-local arena, sink, and context. All HIR nodes and diagnostics
  // produced during body lowering are isolated in body-local storage.
  hir::Arena body_arena;
  DiagnosticSink body_sink;
  Context body_ctx = ctx->ForkForBodyLowering(body_arena, body_sink);

  ModuleLowerer lowerer(body_ctx, registrar, representative);

  SourceSpan span = body_ctx.SpanOf(GetSourceRange(representative));

  // All module-level symbols are pre-registered in Phase 0. Body lowering
  // consumes prepared ownership-shaped input only. Process/function-local
  // symbols are registered during behavioral lowering below.

  std::vector<hir::ProcessId> processes;
  std::vector<hir::FunctionId> functions;
  std::vector<hir::TaskId> tasks;

  {
    // Module scope for behavioral lowering (expression/process/function
    // lowering registers local symbols within this scope context).
    ScopeGuard scope_guard(registrar, ScopeKind::kModule);

    // Lower processes
    for (const auto* proc : input.processes) {
      hir::ProcessId id = LowerProcess(*proc, lowerer);
      if (id) {
        processes.push_back(id);
      }
    }

    // Desugar continuous assignments into synthetic always_comb
    for (const auto* ca : input.continuous_assigns) {
      SourceSpan ca_span = body_ctx.SpanOf(GetSourceRange(*ca));

      if (const auto* delay = ca->getDelay()) {
        body_ctx.sink->Error(
            body_ctx.SpanOf(delay->sourceRange),
            "continuous assignment delays not supported");
        continue;
      }

      const auto& assign_expr = ca->getAssignment();
      const auto& assign = assign_expr.as<slang::ast::AssignmentExpression>();

      hir::ExpressionId target = LowerScopedExpression(
          assign.left(), body_ctx, registrar, lowerer.Frame());
      if (!target) {
        continue;
      }

      const auto& target_data = (*body_ctx.hir_arena)[target];
      if (!hir::IsPlaceExpressionKind(target_data.kind)) {
        body_ctx.sink->Error(
            target_data.span,
            "continuous assignment target is not a simple place expression");
        continue;
      }

      hir::ExpressionId value = LowerScopedExpression(
          assign.right(), body_ctx, registrar, lowerer.Frame());
      if (!value) {
        continue;
      }

      hir::StatementId stmt = body_ctx.hir_arena->AddStatement(
          hir::Statement{
              .kind = hir::StatementKind::kAssignment,
              .span = ca_span,
              .data =
                  hir::AssignmentStatementData{
                      .target = target,
                      .value = value,
                  },
          });

      hir::ProcessId proc = body_ctx.hir_arena->AddProcess(
          hir::Process{
              .kind = hir::ProcessKind::kAlwaysComb,
              .span = ca_span,
              .body = stmt,
          });
      processes.push_back(proc);
    }

    // Lower function bodies
    for (const auto* sub : input.functions) {
      if (!registrar.Lookup(*sub)) {
        continue;
      }
      hir::FunctionId id = LowerFunction(*sub, lowerer);
      if (id) {
        functions.push_back(id);
      }
    }

    // Lower tasks
    for (const auto* sub : input.tasks) {
      hir::TaskId id = LowerTask(*sub, lowerer);
      if (id) {
        tasks.push_back(id);
      }
    }

    // Generate synthetic init process from prepared variable initializers.
    // var_inits are (SymbolId, Expression*) tuples prepared by the caller
    // from the representative's Phase 0 registration context.
    if (!input.var_inits.empty()) {
      std::vector<hir::StatementId> init_stmts;
      init_stmts.reserve(input.var_inits.size());

      for (const auto& var_init : input.var_inits) {
        hir::ExpressionId init_expr = LowerScopedExpression(
            *var_init.initializer, body_ctx, registrar, lowerer.Frame());
        if (!init_expr) {
          continue;
        }

        TypeId var_type = (*body_ctx.symbol_table)[var_init.target].type;
        hir::ExpressionId target_expr = body_ctx.hir_arena->AddExpression(
            hir::Expression{
                .kind = hir::ExpressionKind::kNameRef,
                .type = var_type,
                .span = span,
                .data = hir::NameRefExpressionData{.symbol = var_init.target},
            });

        hir::StatementId stmt = body_ctx.hir_arena->AddStatement(
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
        hir::StatementId body_stmt = body_ctx.hir_arena->AddStatement(
            hir::Statement{
                .kind = hir::StatementKind::kBlock,
                .span = span,
                .data =
                    hir::BlockStatementData{
                        .statements = std::move(init_stmts)},
            });

        hir::ProcessId init_proc = body_ctx.hir_arena->AddProcess(
            hir::Process{
                .kind = hir::ProcessKind::kInitial,
                .span = span,
                .body = body_stmt,
            });

        processes.insert(processes.begin(), init_proc);
      }
    }
  }

  return BodyLoweringResult{
      .body =
          hir::ModuleBody{
              .processes = std::move(processes),
              .functions = std::move(functions),
              .tasks = std::move(tasks),
              .arena = std::move(body_arena),
          },
      .diagnostics = std::move(body_sink).TakeDiagnostics(),
  };
}

auto CollectModuleInstance(
    const slang::ast::InstanceSymbol& instance,
    const InstanceRegistrationInput& input, SymbolRegistrar& registrar,
    Context* ctx, common::ModuleDefId module_def_id, hir::ModuleBodyId body_id)
    -> hir::Module {
  SourceSpan span = ctx->SpanOf(GetSourceRange(instance));

  // Instance symbol must be pre-registered in Phase 0.
  SymbolId symbol = registrar.Lookup(instance);
  if (!symbol) {
    throw common::InternalError(
        "CollectModuleInstance",
        std::format(
            "instance '{}' not pre-registered in Phase 0", instance.name));
  }

  std::vector<SymbolId> variables;
  std::vector<SymbolId> nets;
  std::vector<SymbolId> param_slots;
  std::vector<IntegralConstant> param_init_values;

  // Collect per-instance SymbolIds from prepared registration input.
  for (const auto* var : input.variables) {
    SymbolId sym = registrar.Lookup(*var);
    if (sym) {
      variables.push_back(sym);
    }
  }

  for (const auto* net : input.nets) {
    SymbolId sym = registrar.Lookup(*net);
    if (sym) {
      nets.push_back(sym);
    }
  }

  // Collect promoted parameters and per-instance constant values
  for (const auto* param : input.parameters) {
    SymbolId sym = registrar.Lookup(*param);
    if (sym && (*ctx->symbol_table)[sym].storage_class ==
                   StorageClass::kDesignStorage) {
      param_slots.push_back(sym);
      param_init_values.push_back(
          LowerSVIntToIntegralConstant(param->getValue().integer()));
    }
  }

  return hir::Module{
      .symbol = symbol,
      .span = span,
      .variables = std::move(variables),
      .nets = std::move(nets),
      .param_slots = std::move(param_slots),
      .param_init_values = std::move(param_init_values),
      .module_def_id = module_def_id,
      .body_id = body_id,
  };
}

}  // namespace lyra::lowering::ast_to_hir
