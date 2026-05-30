#pragma once

#include <slang/ast/symbols/MemberSymbols.h>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/continuous_assign.hpp"
#include "lyra/lowering/ast_to_hir/facts.hpp"
#include "lyra/lowering/ast_to_hir/state.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerContinuousAssign(
    const UnitLoweringFacts& unit_facts, ScopeLoweringState& scope_state,
    ScopeStack& stack, const slang::ast::ContinuousAssignSymbol& sym)
    -> diag::Result<hir::ContinuousAssign>;

}  // namespace lyra::lowering::ast_to_hir
