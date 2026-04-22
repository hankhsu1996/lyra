#pragma once

#include <unordered_map>
#include <vector>

#include "lyra/common/symbol_types.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/hir/declaration.hpp"
#include "lyra/hir/fwd.hpp"

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

struct Context;
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

// Legacy flat walk: recursively collects module members, walking
// transparently through generate blocks. Used for per-instance Phase 0
// SymbolId registration and per-spec-group input preparation.
void CollectScopeMembers(
    const slang::ast::Scope& scope, SymbolRegistrar& registrar,
    CollectedMembers& out);

// Region-tree walk. Allocates body-owned decl objects at their
// encounter point, emits one ConditionalGenerateConstruct per
// grouped if/case construct, one LoopGenerateConstruct per
// generate-for. Processes/functions/tasks are NOT region items in
// this cut. `sym_to_decl` is the Cut-1 -> Cut-2 SymbolId bridge;
// see Context::sym_to_decl for lifetime rules.
void BuildRegionTreeFromScope(
    const slang::ast::Scope& scope, hir::GenerateRegionId parent_region_id,
    SymbolRegistrar& registrar, Context* body_ctx, hir::Arena& body_arena,
    std::unordered_map<SymbolId, hir::DeclRef, SymbolIdHash>& sym_to_decl);

}  // namespace lyra::lowering::ast_to_hir
