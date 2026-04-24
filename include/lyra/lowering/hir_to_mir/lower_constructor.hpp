#pragma once

#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/hir_to_mir/facts.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

void LowerConstructorFromScope(
    const UnitLoweringFacts& unit_facts, const UnitLoweringState& unit_state,
    const hir::StructuralScope& root_scope, mir::Body& out_body);

}  // namespace lyra::lowering::hir_to_mir
