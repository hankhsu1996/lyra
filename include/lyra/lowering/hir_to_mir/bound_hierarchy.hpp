#pragma once

#include <cstdint>
#include <optional>
#include <span>
#include <unordered_map>
#include <vector>

#include "lyra/common/local_slot_id.hpp"
#include "lyra/common/module_identity.hpp"
#include "lyra/common/symbol_types.hpp"
#include "lyra/mir/external_ref.hpp"
#include "lyra/mir/handle.hpp"

namespace lyra::hir {
struct Module;
}

namespace lyra::mir {
class Arena;
struct ConstructionInput;
struct Design;
}  // namespace lyra::mir

namespace lyra::lowering::hir_to_mir {

struct BodyLocalSlotEntry;
struct ProvisionalNonLocalTarget;

// Child instance info for parent-child topology.
// Used by parent_to_children map and BoundHierarchyIndex.
struct ChildSiteInfo {
  SymbolId child_instance_sym;
  uint32_t child_module_index = 0;
  common::ModuleSpecId child_spec = {};
};

// Topology index for resolving external refs and connections against the
// construction object graph. Built once from existing lowering data.
// All lookups validate parent-child structure to prevent accidental binding
// to unrelated same-named instances.
struct BoundHierarchyIndex {
  // object_index -> parent object_index (UINT32_MAX for top-level)
  std::vector<uint32_t> parent_of;
  // body_group -> representative object_index
  std::unordered_map<uint32_t, uint32_t> rep_object_for_body;
  // parent_oi -> {child_instance_sym -> child_oi}
  std::unordered_map<
      uint32_t, std::unordered_map<SymbolId, uint32_t, SymbolIdHash>>
      children_of;

  auto WalkUp(uint32_t oi, uint32_t count) const -> uint32_t;
  auto ResolveChildObject(uint32_t parent_oi, SymbolId child_instance_sym) const
      -> uint32_t;
};

auto BuildBoundHierarchyIndex(
    const mir::ConstructionInput& construction,
    const std::unordered_map<uint32_t, std::vector<ChildSiteInfo>>&
        parent_to_children,
    const std::unordered_map<uint32_t, uint32_t>& body_to_representative)
    -> BoundHierarchyIndex;

// Resolve a parent-side NameRef symbol to its body-local LocalSlotId.
// Searches body_slots by SymbolId. Returns nullopt if not found.
auto ResolveParentSourceToLocalSlot(
    SymbolId sym, const std::vector<BodyLocalSlotEntry>& body_slots)
    -> std::optional<common::LocalSlotId>;

// Canonicalize a per-instance SymbolId to the representative module's
// equivalent by positional matching in variables/nets/param_slots.
// Returns nullopt if the symbol is not found in the instance module.
auto CanonicalizeSymToRepresentative(
    SymbolId instance_sym, const hir::Module& instance_mod,
    const hir::Module& representative_mod) -> std::optional<SymbolId>;

// Single canonical API for cross-body symbol canonicalization.
// Maps target_sym from a concrete instance to the representative's equivalent.
auto CanonicalizeExternalRefTargetSym(
    SymbolId target_sym, uint32_t target_oi, uint32_t rep_oi,
    std::span<const hir::Module* const> hir_modules) -> SymbolId;

// Walk a provisional non-local target path to find the target object index.
// Uses structured parent-child hierarchy walk, not global symbol lookup.
auto WalkProvisionalPath(
    const ProvisionalNonLocalTarget& prov, uint32_t current_oi,
    const BoundHierarchyIndex& topo) -> uint32_t;

// Fill target_slot for every external ref in the design. Mandatory post-pass.
// Throws InternalError on any miss.
void FinalizeExternalRefTargetSlots(
    mir::Design& design,
    const std::unordered_map<uint32_t, std::vector<ProvisionalNonLocalTarget>>&
        provisionals_by_body,
    const std::unordered_map<uint32_t, std::vector<BodyLocalSlotEntry>>&
        body_local_slots_by_body,
    const BoundHierarchyIndex& topo, const mir::ConstructionInput& construction,
    std::span<const hir::Module* const> hir_modules);

// Build per-body resolved external ref bindings and store on each body.
// Produces durable binding facts (object_index + local_slot + type), not
// arena-local PlaceIds. Backend materializes Places on-demand.
// Single-instance specs only: throws if a body with external refs has
// multiple objects (multi-instance resolution requires per-instance binding).
void BuildResolvedExternalRefBindings(
    mir::Design& design,
    const std::unordered_map<uint32_t, std::vector<ProvisionalNonLocalTarget>>&
        provisionals_by_body,
    const BoundHierarchyIndex& topo,
    const mir::ConstructionInput& construction);

// Pre-binding resolution: resolve an ExternalRefId to its design-global slot
// using provisional targets + cross_instance_places. Single source of truth
// for the "target_sym maps to exactly one design-global slot" rule.
// Throws InternalError if the target is not found or is not kDesignGlobal.
auto ResolvePreBindingExternalRefDesignGlobalSlot(
    mir::ExternalRefId ref_id,
    const std::vector<ProvisionalNonLocalTarget>& provisional_targets,
    const std::unordered_map<SymbolId, mir::PlaceId, SymbolIdHash>&
        cross_instance_places,
    const mir::Arena& design_arena) -> uint32_t;

}  // namespace lyra::lowering::hir_to_mir
