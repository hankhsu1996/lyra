#pragma once

#include "lyra/hir/package.hpp"
#include "lyra/lowering/hir_to_mir/context.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"
#include "lyra/lowering/origin_map.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/package.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerPackage(
    const hir::Package& package, const LoweringInput& input,
    mir::Arena& mir_arena, OriginMap* origin_map,
    const DesignDeclarations& decls) -> mir::Package;

}  // namespace lyra::lowering::hir_to_mir
