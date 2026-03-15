#pragma once

#include <vector>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/hir/module_body.hpp"
#include "lyra/lowering/hir_to_mir/context.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"
#include "lyra/lowering/origin_map.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/module_body.hpp"

namespace lyra::lowering::hir_to_mir {

// Product of per-body HIR->MIR lowering.
// Contains the body unit with its body-local MIR arena and body-local
// origin entries. Diagnostics use the existing Result<T> fatal-error model.
struct MirBodyLoweringResult {
  mir::ModuleBody body;
  std::vector<OriginEntry> origins;
};

// Lower specialization-shared behavioral content for one spec group.
// The body arena must be pre-populated by CollectBodyLocalDecls before
// this call. All body-local MIR is written into body_arena.
// Cross-domain references (design-global storage, package functions) are
// resolved through the design arena -- LookupPlace creates body-local
// Places with kDesignGlobal roots lazily.
auto LowerModule(
    const hir::ModuleBody& body, const LoweringInput& input,
    mir::Arena body_arena, const mir::Arena& design_arena,
    const DesignDeclarations& decls, const BodyLocalDecls& body_decls,
    hir::ModuleBodyId body_id) -> Result<MirBodyLoweringResult>;

}  // namespace lyra::lowering::hir_to_mir
