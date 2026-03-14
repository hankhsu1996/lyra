#pragma once

#include <slang/ast/symbols/InstanceSymbols.h>

#include "lyra/common/module_identity.hpp"
#include "lyra/hir/module.hpp"
#include "lyra/hir/module_body.hpp"

namespace lyra::lowering::ast_to_hir {

struct CollectedMembers;
struct Context;
class SymbolRegistrar;

// Lower the specialization-shared behavioral content for one specialization
// group. Called once per specialization group.
//
// The representative instance is an input vehicle for timescale extraction
// (ModuleLowerer) and source span generation, not for identity. The output
// must not depend on representative-specific state. Module-level symbols are
// pre-registered in Phase 0; body lowering reads them via Lookup, not
// Register. Members are pre-collected by the caller.
auto LowerModuleBody(
    const slang::ast::InstanceSymbol& instance, const CollectedMembers& members,
    SymbolRegistrar& registrar, Context* ctx) -> hir::ModuleBody;

// Collect per-instance registration and realization data for one instance.
// No behavioral lowering. All symbols are looked up from Phase 0 context.
// Returns a fully-formed instance record.
auto CollectModuleInstance(
    const slang::ast::InstanceSymbol& instance, const CollectedMembers& members,
    SymbolRegistrar& registrar, Context* ctx, common::ModuleDefId module_def_id,
    hir::ModuleBodyId body_id) -> hir::Module;

}  // namespace lyra::lowering::ast_to_hir
