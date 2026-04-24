#pragma once

#include <slang/ast/symbols/BlockSymbols.h>

#include "facts.hpp"
#include "lyra/hir/process.hpp"
#include "state.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerProcess(
    const ScopeLoweringFacts& scope_facts, ScopeLoweringState& scope_state,
    ScopeStack& stack, const slang::ast::ProceduralBlockSymbol& proc)
    -> hir::Process;

}  // namespace lyra::lowering::ast_to_hir
