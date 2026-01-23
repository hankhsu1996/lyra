#pragma once

#include "lyra/hir/module.hpp"
#include "lyra/lowering/hir_to_mir/context.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"
#include "lyra/lowering/origin_map.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/module.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerModule(
    const hir::Module& module, const LoweringInput& input,
    mir::Arena& mir_arena, OriginMap* origin_map,
    const DesignDeclarations& decls) -> mir::Module;

}  // namespace lyra::lowering::hir_to_mir
