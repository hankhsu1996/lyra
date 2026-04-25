#pragma once

#include <slang/ast/symbols/BlockSymbols.h>

#include "facts.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/process.hpp"
#include "state.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerProcess(
    const UnitLoweringFacts& unit_facts, ScopeLoweringState& scope_state,
    ScopeStack& stack, const slang::ast::ProceduralBlockSymbol& proc)
    -> diag::Result<hir::Process>;

}  // namespace lyra::lowering::ast_to_hir
