#pragma once

#include <cstdint>
#include <string>
#include <vector>

#include "lyra/mir/arena.hpp"
#include "lyra/mir/compiled_module_header.hpp"
#include "lyra/mir/connection_recipe.hpp"
#include "lyra/mir/constructor.hpp"
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

  // Body-owned construction entry routine. Exactly one per body. Populated
  // by HIR->MIR constructor lowering.
  Constructor constructor;

  // Opt-in: when true, this body's child construction is executed by a
  // backend-emitted body-constructor function derived from `constructor`.
  // The flat replay path in LyraConstructorRunProgram skips child creation
  // for entries whose parent body carries this flag; the parent's emitted
  // body-constructor has already created those children through a typed
  // runtime helper (LyraConstructorAddChildObject).
  //
  // This is a narrow opt-in: only bodies whose constructor body matches the
  // minimal plain-child new_object pattern (no ports, no bindings, no
  // external refs, no generate, no parameter payloads) may set this flag.
  // Any uncertainty must leave it false so the old flat path remains
  // authoritative. Detection lives in hir_to_mir/module.cpp.
  bool uses_mir_constructor = false;

  // Body-local slot descriptors, indexed by kModuleSlot id.
  // This is the body's required storage interface: what slots exist,
  // their kinds, and their types. This is NOT placement/layout metadata.
  std::vector<SlotDesc> slots;

  // Body-local trace names, parallel to slots (indexed by kModuleSlot id).
  // Source: sym.name from CollectBodyLocalDecls -> BodyLocalDecls -> here.
  // Invariant: local_trace_names.size() == slots.size().
  std::vector<std::string> local_trace_names;

  // Body-local named event descriptors. Indexed by body-local EventId.
  std::vector<EventDesc> events;

  // Per-body timescale from source-level scope metadata.
  // Set during HIR-to-MIR lowering from body_timescales.
  int8_t time_unit_power = 0;
  int8_t time_precision_power = 0;

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

  // Body-local public port entries. Populated from CompiledModuleHeader
  // during design lowering. Emitted to runtime for constructor-side
  // symbolic port -> local slot resolution.
  // Uses the canonical mir::PortEntry from compiled_module_header.hpp.
  std::vector<mir::PortEntry> port_entries;

  // B2: Child instantiation sites for this body.
  std::vector<ChildInstantiationSite> child_sites;

  // B2: Connection recipes for this body.
  std::vector<ConnectionRecipe> connection_recipes;

  // Installable computation templates for all reactive parent->child
  // port bindings. One template per binding; source shape (plain slot
  // read vs arbitrary expression) is erased upstream, so every entry
  // here is a uniform target + callable + deps. Created by
  // BuildInstallableComputations. Not processes. kDriveChildToParent
  // recipes remain on the memcpy connection path and do not appear here.
  std::vector<InstallableComputationTemplate> installable_computations;

  // Body-owned cover sites. CoverSiteId in MIR effects is body-local
  // (0-based per body). The design-global table is built by concatenation
  // at assembly time.
  std::vector<ImmediateCoverSiteInfo> immediate_cover_sites;

  // Body-owned deferred assertion sites. DeferredAssertionSiteId in MIR
  // effects is body-local (0-based per body). The design-global table is
  // built by concatenation at assembly time.
  std::vector<DeferredAssertionSiteInfo> deferred_assertion_sites;
};

// Helper to get the number of installable computations in a body.
[[nodiscard]] inline auto GetInstallableComputationCount(const ModuleBody& body)
    -> uint32_t {
  return static_cast<uint32_t>(body.installable_computations.size());
}

}  // namespace lyra::mir
