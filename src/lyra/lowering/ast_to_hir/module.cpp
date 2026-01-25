#include "lyra/lowering/ast_to_hir/module.hpp"

#include <format>
#include <span>
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
#include "lyra/lowering/ast_to_hir/port_binding.hpp"
#include "lyra/lowering/ast_to_hir/routine.hpp"
#include "lyra/lowering/ast_to_hir/source_utils.hpp"
#include "lyra/lowering/ast_to_hir/symbol_registrar.hpp"
#include "lyra/lowering/ast_to_hir/type.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerModule(
    const slang::ast::InstanceSymbol& instance, SymbolRegistrar& registrar,
    Context* ctx, std::span<const InputPortBinding> port_bindings)
    -> hir::Module {
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
      hir::ProcessId id = LowerProcess(*proc, registrar, ctx);
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

    // Phase 3c: Input port bindings -> synthetic always_comb drivers
    // These drive child port variables from parent-scope expressions.
    // Scheduling is slot-global: any slot write wakes all waiters for that
    // slot.
    for (const auto& binding : port_bindings) {
      SourceSpan binding_span = ctx->SpanOf(binding.connection_range);

      // Look up target variable's SymbolId
      // IMPORTANT: This relies on Phase 0 having registered the child's vars.
      SymbolId target_sym = registrar.Lookup(*binding.target_var);
      if (!target_sym) {
        ctx->sink->Error(
            binding_span, std::format(
                              "port binding target '{}' not registered - "
                              "port backing variable may not be a scope member",
                              binding.target_var->name));
        continue;
      }

      // Get target type FROM THE REGISTERED SYMBOL (not re-lowering slang type)
      // This avoids divergence from Phase 0 registration (params,
      // specialization)
      TypeId target_type = (*ctx->symbol_table)[target_sym].type;
      if (!target_type) {
        ctx->sink->Error(binding_span, "port binding target has invalid type");
        continue;
      }

      // Create hierarchical ref for target (child's port variable)
      // NOTE: kHierarchicalRef lowering uses LookupPlace(sym_id) which is a
      // global map lookup, NOT scope-relative. This ensures correct slot
      // resolution even with multiple instances of the same module definition.
      hir::ExpressionId target_expr = ctx->hir_arena->AddExpression(
          hir::Expression{
              .kind = hir::ExpressionKind::kHierarchicalRef,
              .type = target_type,
              .span = binding_span,
              .data = hir::HierarchicalRefExpressionData{.target = target_sym},
          });

      // Lower connection expression (parent scope)
      hir::ExpressionId value_expr =
          LowerExpression(*binding.value_expr, registrar, ctx);
      if (!value_expr) {
        continue;
      }

      // Create assignment statement: child.port_var = parent_expr
      hir::StatementId stmt = ctx->hir_arena->AddStatement(
          hir::Statement{
              .kind = hir::StatementKind::kAssignment,
              .span = binding_span,
              .data =
                  hir::AssignmentStatementData{
                      .target = target_expr,
                      .value = value_expr,
                  },
          });

      // Wrap in always_comb process (same pattern as continuous assignments)
      hir::ProcessId port_proc = ctx->hir_arena->AddProcess(
          hir::Process{
              .kind = hir::ProcessKind::kAlwaysComb,
              .span = binding_span,
              .body = stmt,
          });
      processes.push_back(port_proc);
    }

    // Phase 4: Lower function bodies
    for (const auto* sub : members.functions) {
      const auto& ret_type = sub->getReturnType();
      if (!ret_type.isIntegral() && !ret_type.isVoid()) {
        continue;
      }
      hir::FunctionId id = LowerFunction(*sub, registrar, ctx);
      if (id) {
        functions.push_back(id);
      }
    }

    // Phase 5: Lower tasks
    for (const auto* sub : members.tasks) {
      hir::TaskId id = LowerTask(*sub, registrar, ctx);
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
