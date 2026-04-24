#pragma once

#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/facts.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerStmtData(
    const UnitLoweringFacts& unit_facts, const UnitLoweringState& unit_state,
    const ScopeStack& stack, const BodyLoweringState& body_state,
    const hir::StmtData& data) -> mir::StmtData;

}  // namespace lyra::lowering::hir_to_mir
