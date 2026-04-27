#pragma once

#include "lyra/hir/process.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/process.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerProcess(
    const UnitLoweringState& unit_state, const ClassLoweringState& class_state,
    const hir::StructuralScope& process_scope, const hir::Process& src)
    -> mir::Process;

}  // namespace lyra::lowering::hir_to_mir
