#pragma once

#include "lyra/hir/process.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/process.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerProcessData(
    const ProcessLoweringState& process_state, const hir::ProcessData& data)
    -> mir::ProcessData;

auto LowerProcess(const UnitLoweringState& unit_state, const hir::Process& src)
    -> mir::Process;

}  // namespace lyra::lowering::hir_to_mir
