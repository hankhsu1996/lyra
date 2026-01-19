#include "lyra/lowering/ast_to_hir/module.hpp"

#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/VariableSymbols.h>

#include "lyra/lowering/ast_to_hir/context.hpp"
#include "lyra/lowering/ast_to_hir/routine.hpp"
#include "lyra/lowering/ast_to_hir/source_utils.hpp"
#include "lyra/lowering/ast_to_hir/symbol_registrar.hpp"
#include "lyra/lowering/ast_to_hir/type.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerModule(
    const slang::ast::InstanceSymbol& instance, SymbolRegistrar& registrar,
    Context* ctx) -> hir::Module {
  const slang::ast::InstanceBodySymbol& body = instance.body;
  SourceSpan span = ctx->SpanOf(GetSourceRange(instance));

  SymbolId symbol =
      registrar.Register(instance, SymbolKind::kModule, kInvalidTypeId);

  std::vector<SymbolId> variables;
  std::vector<hir::ProcessId> processes;
  std::vector<hir::FunctionId> functions;
  std::vector<hir::TaskId> tasks;

  {
    ScopeGuard scope_guard(registrar, ScopeKind::kModule);

    // Phase 1: Register module-level variables (before processes that use them)
    for (const slang::ast::VariableSymbol& var :
         body.membersOfType<slang::ast::VariableSymbol>()) {
      TypeId type = LowerType(var.getType(), span, ctx);
      if (type) {
        SymbolId sym = registrar.Register(var, SymbolKind::kVariable, type);
        variables.push_back(sym);
      }
    }

    // Phase 2: Register function symbols (before processes that call them)
    // Store references to lower their bodies later
    std::vector<const slang::ast::SubroutineSymbol*> function_refs;
    std::vector<const slang::ast::SubroutineSymbol*> task_refs;
    for (const slang::ast::SubroutineSymbol& sub :
         body.membersOfType<slang::ast::SubroutineSymbol>()) {
      if (sub.subroutineKind == slang::ast::SubroutineKind::Function) {
        // Register function symbol only (return type, not body yet)
        const auto& ret_type = sub.getReturnType();
        if (!ret_type.isIntegral() && !ret_type.isVoid()) {
          ctx->sink->Error(
              span, "only integral or void return types supported");
          continue;
        }
        TypeId return_type = LowerType(ret_type, span, ctx);
        if (!return_type) {
          continue;
        }
        registrar.Register(sub, SymbolKind::kFunction, return_type);
        function_refs.push_back(&sub);
      } else {
        task_refs.push_back(&sub);
      }
    }

    // Phase 3: Lower processes (can now reference function symbols)
    for (const slang::ast::ProceduralBlockSymbol& proc :
         body.membersOfType<slang::ast::ProceduralBlockSymbol>()) {
      hir::ProcessId id = LowerProcess(proc, registrar, ctx);
      if (id) {
        processes.push_back(id);
      }
    }

    // Phase 4: Lower function bodies
    for (const slang::ast::SubroutineSymbol* sub : function_refs) {
      hir::FunctionId id = LowerFunction(*sub, registrar, ctx);
      if (id) {
        functions.push_back(id);
      }
    }

    // Phase 5: Lower tasks
    for (const slang::ast::SubroutineSymbol* sub : task_refs) {
      hir::TaskId id = LowerTask(*sub, registrar, ctx);
      if (id) {
        tasks.push_back(id);
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
