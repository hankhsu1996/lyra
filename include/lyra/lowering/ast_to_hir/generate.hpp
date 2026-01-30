#pragma once

#include <vector>

namespace slang::ast {
class Scope;
class VariableSymbol;
class NetSymbol;
class ParameterSymbol;
class SubroutineSymbol;
class ProceduralBlockSymbol;
class ContinuousAssignSymbol;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

class SymbolRegistrar;

struct CollectedMembers {
  std::vector<const slang::ast::VariableSymbol*> variables;
  std::vector<const slang::ast::NetSymbol*> nets;
  std::vector<const slang::ast::ParameterSymbol*> parameters;
  std::vector<const slang::ast::SubroutineSymbol*> functions;
  std::vector<const slang::ast::SubroutineSymbol*> tasks;
  std::vector<const slang::ast::ProceduralBlockSymbol*> processes;
  std::vector<const slang::ast::ContinuousAssignSymbol*> continuous_assigns;
};

// Recursively collect module members from a scope, walking into
// instantiated generate blocks. Pushes kGenerate scopes for ancestry.
void CollectScopeMembers(
    const slang::ast::Scope& scope, SymbolRegistrar& registrar,
    CollectedMembers& out);

}  // namespace lyra::lowering::ast_to_hir
