#pragma once

#include <span>

#include <slang/ast/symbols/BlockSymbols.h>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/ast_to_hir/facts.hpp"
#include "lyra/lowering/ast_to_hir/state.hpp"

namespace lyra::lowering::ast_to_hir {

auto BuildIfGenerate(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ScopeLoweringState& parent_state, ScopeStack& stack,
    std::span<const slang::ast::GenerateBlockSymbol* const> siblings)
    -> diag::Result<hir::Generate>;

auto BuildCaseGenerate(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ScopeLoweringState& parent_state, ScopeStack& stack,
    std::span<const slang::ast::GenerateBlockSymbol* const> siblings)
    -> diag::Result<hir::Generate>;

auto BuildLoopGenerate(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ScopeLoweringState& parent_state, ScopeStack& stack,
    const slang::ast::GenerateBlockArraySymbol& array)
    -> diag::Result<hir::Generate>;

}  // namespace lyra::lowering::ast_to_hir
