#pragma once

#include <vector>

#include "lyra/mir/arena.hpp"
#include "lyra/mir/connection_recipe.hpp"
#include "lyra/mir/cover_site.hpp"
#include "lyra/mir/deferred_assertion_site.hpp"
#include "lyra/mir/external_ref.hpp"
#include "lyra/mir/handle.hpp"

namespace lyra::mir {

enum class SlotKind : uint8_t;
struct SlotDesc;
struct EventDesc;

// Implementation-detail body container, pending migration to
// CompiledModuleBody (compiled_specialization.hpp) in B2.
//
// CompiledModuleBody is the ONLY public specialization-scoped body
// artifact type going forward. New code must not depend on this type
// as a public contract. Existing consumers will be migrated in B2.
//
// Owns all body-local MIR storage: processes, functions, places, and
// slot descriptors. ProcessIds, FunctionIds, and body-local PlaceIds are
// indices into the embedded arena. They are permanently body-local.
//
// Invariants:
// - No instance identity (no instance_sym, no instance path strings)
// - No placement/layout ownership (no slot offsets)
// - No assembly-scoped data (no binding/connectivity metadata)
// - No design-global routing metadata
// - All body-local IDs resolve through arena (no rebasing)
struct ModuleBody {
  std::vector<ProcessId> processes;
  std::vector<FunctionId> functions;

  // Body-local slot descriptors, indexed by kModuleSlot id.
  // This is the body's required storage interface: what slots exist,
  // their kinds, and their types. This is NOT placement/layout metadata.
  std::vector<SlotDesc> slots;

  // Body-local named event descriptors. Indexed by body-local EventId.
  std::vector<EventDesc> events;

  // Total body-global decision site count. Set by module lowering after all
  // functions, processes, and tasks are lowered. Defines the authoritative
  // size of the body-wide decision metadata table. The LLVM backend validates
  // that exactly this many sites are reconstructed from the stored records.
  uint32_t total_decision_sites = 0;

  // Body-local MIR storage. All body-local PlaceIds, ProcessIds, and
  // FunctionIds are indices into this arena.
  Arena arena;

  // B2: External access recipes for this body.
  // Specialization-scoped: contains only instance-invariant data (upward_count,
  // canonical durable child path, target_local_slot, type, access_kind).
  // No concrete target_object. Per-instance resolution happens at construction
  // time via ConstructionInput::instance_ext_ref_bindings.
  std::vector<ExternalAccessRecipe> external_refs;

  // B2: Child instantiation sites for this body.
  std::vector<ChildInstantiationSite> child_sites;

  // B2: Connection recipes for this body.
  std::vector<ConnectionRecipe> connection_recipes;

  // Body-owned cover sites. CoverSiteId in MIR effects is body-local
  // (0-based per body). The design-global table is built by concatenation
  // at assembly time.
  std::vector<ImmediateCoverSiteInfo> immediate_cover_sites;

  // Body-owned deferred assertion sites. DeferredAssertionSiteId in MIR
  // effects is body-local (0-based per body). The design-global table is
  // built by concatenation at assembly time.
  std::vector<DeferredAssertionSiteInfo> deferred_assertion_sites;
};

}  // namespace lyra::mir
