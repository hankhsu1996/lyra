#pragma once

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/hir_to_mir/facts.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerConstructorIntoBody(
    const UnitLoweringFacts& unit_facts, const UnitLoweringState& unit_state,
    const hir::StructuralScope& root_scope, mir::Body& out_body)
    -> diag::Result<void>;

}  // namespace lyra::lowering::hir_to_mir
