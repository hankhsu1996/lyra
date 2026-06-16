#pragma once

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/continuous_assign.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/process.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerContinuousAssign(
    const StructuralScopeLowerer& scope, WalkFrame frame,
    const hir::ContinuousAssign& src) -> diag::Result<mir::Process>;

}  // namespace lyra::lowering::hir_to_mir
