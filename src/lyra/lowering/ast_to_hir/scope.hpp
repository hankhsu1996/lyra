#pragma once

#include <slang/ast/Scope.h>

#include "facts.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "state.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerScopeInto(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    hir::StructuralScope& scope, const slang::ast::Scope& slang_scope,
    ScopeStack& stack) -> diag::Result<void>;

}  // namespace lyra::lowering::ast_to_hir
