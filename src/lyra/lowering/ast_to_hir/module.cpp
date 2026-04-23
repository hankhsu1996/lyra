#include "module.hpp"

#include <string>

#include <slang/ast/Scope.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/VariableSymbols.h>

#include "facts.hpp"
#include "lyra/hir/module_unit.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/support/unsupported.hpp"
#include "process.hpp"
#include "state.hpp"
#include "type.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerModule(const slang::ast::InstanceBodySymbol& body)
    -> hir::ModuleUnit {
  ModuleLoweringState state{std::string{body.name}};
  const ModuleLoweringFacts facts(body);

  for (const auto& member : body.members()) {
    switch (member.kind) {
      case slang::ast::SymbolKind::Variable: {
        const auto& var = member.as<slang::ast::VariableSymbol>();
        const hir::TypeId type_id = LowerType(state, var.getType());
        state.AddVariableDeclBinding(var, type_id);
        break;
      }
      case slang::ast::SymbolKind::ProceduralBlock:
        break;
      default:
        break;
    }
  }

  for (const auto& member : body.members()) {
    if (member.kind != slang::ast::SymbolKind::ProceduralBlock) {
      continue;
    }
    const auto& proc = member.as<slang::ast::ProceduralBlockSymbol>();
    if (proc.procedureKind != slang::ast::ProceduralBlockKind::Initial) {
      support::Unsupported("LowerModule: only `initial` processes supported");
    }
    state.AddProcess(LowerProcess(facts, state, proc));
  }

  return state.MoveHirUnit();
}

}  // namespace lyra::lowering::ast_to_hir
