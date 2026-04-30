#pragma once

#include <string>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/structural_scope.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerStructuralScope(
    UnitLoweringState& unit_state,
    const StructuralScopeLoweringState* parent_scope_state, ScopeStack& stack,
    const hir::StructuralScope& scope, std::string name)
    -> diag::Result<mir::StructuralScope>;

}  // namespace lyra::lowering::hir_to_mir
