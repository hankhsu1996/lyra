#include "lyra/lowering/ast_to_hir/generate.hpp"

#include <cstddef>
#include <format>
#include <string>

#include <slang/ast/Symbol.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/MemberSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/ValueSymbol.h>
#include <slang/ast/symbols/VariableSymbols.h>

#include "lyra/common/scope_types.hpp"
#include "lyra/lowering/ast_to_hir/symbol_registrar.hpp"

namespace lyra::lowering::ast_to_hir {

void CollectScopeMembers(
    const slang::ast::Scope& scope, SymbolRegistrar& registrar,
    CollectedMembers& out) {
  for (const auto& member : scope.members()) {
    switch (member.kind) {
      case slang::ast::SymbolKind::Variable: {
        const auto& var = member.as<slang::ast::VariableSymbol>();
        out.variables.push_back(&var);
        break;
      }
      case slang::ast::SymbolKind::Net: {
        const auto& net = member.as<slang::ast::NetSymbol>();
        out.nets.push_back(&net);
        break;
      }
      case slang::ast::SymbolKind::Subroutine: {
        const auto& sub = member.as<slang::ast::SubroutineSymbol>();
        if (sub.subroutineKind == slang::ast::SubroutineKind::Function) {
          out.functions.push_back(&sub);
        } else {
          out.tasks.push_back(&sub);
        }
        break;
      }
      case slang::ast::SymbolKind::ProceduralBlock: {
        const auto& proc = member.as<slang::ast::ProceduralBlockSymbol>();
        out.processes.push_back(&proc);
        break;
      }
      case slang::ast::SymbolKind::ContinuousAssign: {
        const auto& ca = member.as<slang::ast::ContinuousAssignSymbol>();
        out.continuous_assigns.push_back(&ca);
        break;
      }
      case slang::ast::SymbolKind::GenerateBlock: {
        const auto& block = member.as<slang::ast::GenerateBlockSymbol>();
        if (block.isUninstantiated) {
          break;
        }
        ScopeGuard guard(
            registrar, ScopeKind::kGenerate, std::string(block.name));
        CollectScopeMembers(block, registrar, out);
        break;
      }
      case slang::ast::SymbolKind::GenerateBlockArray: {
        const auto& array = member.as<slang::ast::GenerateBlockArraySymbol>();
        for (size_t i = 0; i < array.entries.size(); ++i) {
          const auto* entry = array.entries[i];
          if (entry->isUninstantiated) {
            continue;
          }
          ScopeGuard guard(
              registrar, ScopeKind::kGenerate,
              std::format("{}[{}]", array.name, i));
          CollectScopeMembers(*entry, registrar, out);
        }
        break;
      }
      default:
        break;
    }
  }
}

}  // namespace lyra::lowering::ast_to_hir
