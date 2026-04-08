#pragma once

#include <vector>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/hir/module_body.hpp"
#include "lyra/lowering/hir_to_mir/context.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"
#include "lyra/lowering/origin_map.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/external_ref.hpp"
#include "lyra/mir/module_body.hpp"

namespace lyra::lowering::hir_to_mir {

// Product of per-body HIR->MIR lowering.
// Contains the body unit with its body-local MIR arena and body-local
// origin entries. Diagnostics use the existing Result<T> fatal-error model.
struct MirBodyLoweringResult {
  mir::ModuleBody body;
  std::vector<OriginEntry> origins;
  // Body-local symbol-to-function map. Retained for downstream consumers
  // that need to resolve body-owned symbols to body-local FunctionIds
  // (e.g., module-scoped DPI export target resolution).
  SymbolToMirFunctionMap symbol_to_function;
  // B2: External access recipes collected during body lowering.
  std::vector<mir::ExternalAccessRecipe> external_refs;
  // B2: Provisional non-local targets (parallel to external_refs).
  // Consumed by FinalizeExternalRefTargetSlots and
  // BuildResolvedExternalRefPlaces in design_lower.cpp post-passes.
  std::vector<ProvisionalNonLocalTarget> provisional_targets;
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
    hir::ModuleBodyId body_id,
    mir::ImmediateCoverSiteRegistry* cover_site_registry,
    mir::DeferredAssertionSiteRegistry* deferred_assertion_site_registry,
    absl::flat_hash_map<
        mir::DeferredAssertionActionKey, mir::DeferredUserCallRealization>*
        deferred_assertion_realizations,
    const PlaceMap* cross_instance_places = nullptr)
    -> Result<MirBodyLoweringResult>;

}  // namespace lyra::lowering::hir_to_mir
