#pragma once

#include <slang/ast/symbols/BlockSymbols.h>

#include "facts.hpp"
#include "lyra/hir/process.hpp"
#include "state.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerProcess(
    const ModuleLoweringFacts& module_facts,
    const ModuleLoweringState& module_state,
    const slang::ast::ProceduralBlockSymbol& proc) -> hir::Process;

}  // namespace lyra::lowering::ast_to_hir
