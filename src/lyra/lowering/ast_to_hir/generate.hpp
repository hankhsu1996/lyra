#pragma once

#include <span>

#include <slang/ast/symbols/BlockSymbols.h>

#include "lyra/hir/structural_scope.hpp"
#include "state.hpp"

namespace lyra::lowering::ast_to_hir {

auto BuildIfGenerate(
    UnitLoweringState& unit, ScopeLoweringState& parent_state,
    ScopeStack& stack,
    std::span<const slang::ast::GenerateBlockSymbol* const> siblings)
    -> hir::Generate;

auto BuildCaseGenerate(
    UnitLoweringState& unit, ScopeLoweringState& parent_state,
    ScopeStack& stack,
    std::span<const slang::ast::GenerateBlockSymbol* const> siblings)
    -> hir::Generate;

}  // namespace lyra::lowering::ast_to_hir
