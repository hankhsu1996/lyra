#pragma once

#include <vector>

#include <slang/ast/symbols/InstanceSymbols.h>

#include "lyra/common/module_identity.hpp"
#include "lyra/common/symbol_types.hpp"
#include "lyra/hir/module.hpp"
#include "lyra/hir/module_body.hpp"

namespace slang::ast {
class Expression;
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

// Prepared behavioral input for specialization-group body lowering.
// Definition-scoped: identical across all instances in a spec group.
// Consumed only by LowerModuleBody.
struct BodyLoweringInput {
  std::vector<const slang::ast::ProceduralBlockSymbol*> processes;
  std::vector<const slang::ast::ContinuousAssignSymbol*> continuous_assigns;
  std::vector<const slang::ast::SubroutineSymbol*> functions;
  std::vector<const slang::ast::SubroutineSymbol*> tasks;
  // Variable initializers prepared as (target SymbolId, initializer AST expr).
  // SymbolIds resolved from representative's Phase 0 registration.
  struct VarInit {
    SymbolId target;
    const slang::ast::Expression* initializer;
  };
  std::vector<VarInit> var_inits;
};

// Prepared per-instance registration input for instance record collection.
// Per-instance: each elaborated instance has distinct member pointers.
// Consumed only by CollectModuleInstance.
struct InstanceRegistrationInput {
  std::vector<const slang::ast::VariableSymbol*> variables;
  std::vector<const slang::ast::NetSymbol*> nets;
  std::vector<const slang::ast::ParameterSymbol*> parameters;
};

// Lower specialization-shared behavioral content for one spec group.
// Called once per group. The representative instance provides timescale
// (ModuleLowerer) and source span context, not identity.
auto LowerModuleBody(
    const slang::ast::InstanceSymbol& representative,
    const BodyLoweringInput& input, SymbolRegistrar& registrar, Context* ctx)
    -> hir::ModuleBody;

// Collect per-instance registration data. No behavioral lowering.
// All symbols are looked up from Phase 0 context.
// Returns a fully-formed instance record.
auto CollectModuleInstance(
    const slang::ast::InstanceSymbol& instance,
    const InstanceRegistrationInput& input, SymbolRegistrar& registrar,
    Context* ctx, common::ModuleDefId module_def_id, hir::ModuleBodyId body_id)
    -> hir::Module;

}  // namespace lyra::lowering::ast_to_hir
