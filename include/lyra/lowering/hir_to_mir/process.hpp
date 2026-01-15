#pragma once

#include "lyra/hir/routine.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/handle.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerProcess(
    const hir::Process& process, const LoweringInput& input,
    mir::Arena& mir_arena) -> mir::ProcessId;

}  // namespace lyra::lowering::hir_to_mir
