#pragma once

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/hir/design.hpp"
#include "lyra/lowering/hir_to_mir/context.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"
#include "lyra/lowering/origin_map.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/design.hpp"

namespace lyra::lowering::hir_to_mir {

auto CollectDeclarations(
    const hir::Design& design, const LoweringInput& input,
    mir::Arena& mir_arena) -> DesignDeclarations;

auto LowerDesign(
    const hir::Design& design, const LoweringInput& input,
    mir::Arena& mir_arena, OriginMap* origin_map) -> Result<mir::Design>;

}  // namespace lyra::lowering::hir_to_mir
