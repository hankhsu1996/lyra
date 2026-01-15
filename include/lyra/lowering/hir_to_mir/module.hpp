#pragma once

#include "lyra/hir/module.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/module.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerModule(
    const hir::Module& module, const LoweringInput& input,
    mir::Arena& mir_arena) -> mir::Module;

}  // namespace lyra::lowering::hir_to_mir
