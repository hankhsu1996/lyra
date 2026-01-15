#pragma once

#include "lyra/hir/package.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/package.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerPackage(
    const hir::Package& package, const LoweringInput& input,
    mir::Arena& mir_arena) -> mir::Package;

}  // namespace lyra::lowering::hir_to_mir
