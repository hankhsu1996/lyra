#pragma once

#include "lyra/hir/lvalue.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/member.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

auto TranslateLvalueTargetToMember(
    const UnitLoweringState& unit_state, const hir::Lvalue& lvalue)
    -> mir::MemberId;

auto LowerStmtData(
    const UnitLoweringState& unit_state,
    const ProcessLoweringState& process_state, const hir::StmtData& data)
    -> mir::StmtData;

}  // namespace lyra::lowering::hir_to_mir
