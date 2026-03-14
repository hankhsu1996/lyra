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
#include "lyra/lowering/ast_to_hir/generate.hpp"
#include "lyra/lowering/ast_to_hir/module_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/routine.hpp"
#include "lyra/lowering/ast_to_hir/source_utils.hpp"
#include "lyra/lowering/ast_to_hir/symbol_registrar.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerModuleBody(
    const slang::ast::InstanceSymbol& instance, const CollectedMembers& members,
    SymbolRegistrar& registrar, Context* ctx) -> hir::ModuleBody {
  ModuleLowerer lowerer(*ctx, registrar, instance);

  SourceSpan span = ctx->SpanOf(GetSourceRange(instance));

  // Module-level symbols (instance, variables, nets, functions, parameters)
  // are pre-registered in Phase 0. Body lowering only looks up existing
  // registrations. Process/function-local symbols are registered during
  // behavioral lowering below. Members are pre-collected by the caller.

  std::vector<hir::ProcessId> processes;
  std::vector<hir::FunctionId> functions;
  std::vector<hir::TaskId> tasks;

  // Track variables with initializers for init process generation.
  // Variable SymbolIds are looked up from Phase 0 registration context.
  std::vector<std::pair<SymbolId, const slang::ast::Expression*>> var_init_refs;

  {
    // Module scope for behavioral lowering (expression/process/function
    // lowering registers local symbols within this scope context).
    ScopeGuard scope_guard(registrar, ScopeKind::kModule);

    // Look up variable SymbolIds (Phase 0) and collect initializer references
    // for deferred init process generation.
    for (const auto* var : members.variables) {
      SymbolId sym = registrar.Lookup(*var);
      if (sym) {
        if (const auto* init = var->getInitializer()) {
          var_init_refs.emplace_back(sym, init);
        }
      }
    }

    // Lower processes
    for (const auto* proc : members.processes) {
      hir::ProcessId id = LowerProcess(*proc, lowerer);
      if (id) {
        processes.push_back(id);
      }
    }

    // Desugar continuous assignments into synthetic always_comb
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

      hir::ExpressionId target = LowerScopedExpression(
          assign.left(), *ctx, registrar, lowerer.Frame());
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

      hir::ExpressionId value = LowerScopedExpression(
          assign.right(), *ctx, registrar, lowerer.Frame());
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

    // Lower function bodies
    for (const auto* sub : members.functions) {
      if (!registrar.Lookup(*sub)) {
        continue;
      }
      hir::FunctionId id = LowerFunction(*sub, lowerer);
      if (id) {
        functions.push_back(id);
      }
    }

    // Lower tasks
    for (const auto* sub : members.tasks) {
      hir::TaskId id = LowerTask(*sub, lowerer);
      if (id) {
        tasks.push_back(id);
      }
    }

    // Generate synthetic init process for variable initializers
    if (!var_init_refs.empty()) {
      std::vector<hir::StatementId> init_stmts;
      init_stmts.reserve(var_init_refs.size());

      for (const auto& [sym, init_ast] : var_init_refs) {
        hir::ExpressionId init_expr =
            LowerScopedExpression(*init_ast, *ctx, registrar, lowerer.Frame());
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
        hir::StatementId body_stmt = ctx->hir_arena->AddStatement(
            hir::Statement{
                .kind = hir::StatementKind::kBlock,
                .span = span,
                .data =
                    hir::BlockStatementData{
                        .statements = std::move(init_stmts)},
            });

        hir::ProcessId init_proc = ctx->hir_arena->AddProcess(
            hir::Process{
                .kind = hir::ProcessKind::kInitial,
                .span = span,
                .body = body_stmt,
            });

        processes.insert(processes.begin(), init_proc);
      }
    }
  }

  return hir::ModuleBody{
      .processes = std::move(processes),
      .functions = std::move(functions),
      .tasks = std::move(tasks),
  };
}

auto CollectModuleInstance(
    const slang::ast::InstanceSymbol& instance, const CollectedMembers& members,
    SymbolRegistrar& registrar, Context* ctx, common::ModuleDefId module_def_id,
    hir::ModuleBodyId body_id) -> hir::Module {
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

  // Collect per-instance SymbolIds from pre-collected members.
  // All symbols are looked up from Phase 0 registration context.
  for (const auto* var : members.variables) {
    SymbolId sym = registrar.Lookup(*var);
    if (sym) {
      variables.push_back(sym);
    }
  }

  for (const auto* net : members.nets) {
    SymbolId sym = registrar.Lookup(*net);
    if (sym) {
      nets.push_back(sym);
    }
  }

  // Collect promoted parameters and per-instance constant values
  for (const auto* param : members.parameters) {
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
