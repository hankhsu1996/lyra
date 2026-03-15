#pragma once

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/hir/module_body.hpp"
#include "lyra/lowering/hir_to_mir/context.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"
#include "lyra/lowering/origin_map.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/module_body.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerModule(
    const hir::ModuleBody& body, const LoweringInput& input,
    mir::Arena& mir_arena, OriginMap* origin_map,
    const DesignDeclarations& decls, const BodyLocalDecls& body_decls,
    hir::ModuleBodyId body_id) -> Result<mir::ModuleBody>;

}  // namespace lyra::lowering::hir_to_mir
