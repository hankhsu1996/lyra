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

    // Register module-level variables first (before processes that use them)
    for (const slang::ast::VariableSymbol& var :
         body.membersOfType<slang::ast::VariableSymbol>()) {
      TypeId type = LowerType(var.getType(), span, ctx);
      if (type) {
        SymbolId sym = registrar.Register(var, SymbolKind::kVariable, type);
        variables.push_back(sym);
      }
    }

    for (const slang::ast::ProceduralBlockSymbol& proc :
         body.membersOfType<slang::ast::ProceduralBlockSymbol>()) {
      hir::ProcessId id = LowerProcess(proc, registrar, ctx);
      if (id) {
        processes.push_back(id);
      }
    }

    for (const slang::ast::SubroutineSymbol& sub :
         body.membersOfType<slang::ast::SubroutineSymbol>()) {
      if (sub.subroutineKind == slang::ast::SubroutineKind::Function) {
        hir::FunctionId id = LowerFunction(sub, registrar, ctx);
        if (id) {
          functions.push_back(id);
        }
      } else {
        hir::TaskId id = LowerTask(sub, registrar, ctx);
        if (id) {
          tasks.push_back(id);
        }
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
