#pragma once

#include "lyra/hir/design.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/design.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerDesign(
    const hir::Design& design, const LoweringInput& input,
    mir::Arena& mir_arena) -> mir::Design;

}  // namespace lyra::lowering::hir_to_mir
