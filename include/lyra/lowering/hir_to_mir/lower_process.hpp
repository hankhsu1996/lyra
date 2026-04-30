#pragma once

#include "lyra/base/time.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/process.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerProcess(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state, const hir::Process& src,
    TimeResolution time_resolution) -> diag::Result<mir::Process>;

}  // namespace lyra::lowering::hir_to_mir
