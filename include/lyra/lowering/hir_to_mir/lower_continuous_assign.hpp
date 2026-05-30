#pragma once

#include "lyra/base/time.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/continuous_assign.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/process.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerContinuousAssign(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const hir::StructuralScope& scope, const hir::ContinuousAssign& src,
    TimeResolution time_resolution) -> diag::Result<mir::Process>;

}  // namespace lyra::lowering::hir_to_mir
