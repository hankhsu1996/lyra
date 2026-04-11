#pragma once

#include <cstdint>
#include <optional>
#include <span>
#include <unordered_map>
#include <vector>

#include "lyra/common/local_slot_id.hpp"
#include "lyra/common/module_identity.hpp"
#include "lyra/common/object_index.hpp"
#include "lyra/common/symbol_types.hpp"
#include "lyra/mir/external_ref.hpp"

namespace lyra::hir {
struct Module;
}

namespace lyra::mir {
class Arena;
struct BoundConnection;
struct ConnectionRecipe;
struct ConstructionInput;
struct Design;
struct ModuleBody;
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
  // Used by provisional path walking (FinalizeExternalRefTargetSlots).
  std::unordered_map<
      uint32_t, std::unordered_map<SymbolId, uint32_t, SymbolIdHash>>
      children_of;
  // parent_body_group -> {DurableChildId -> child_oi}
  // Used by canonical path walking (WalkCanonicalPath).
  // Keyed by body_group (not oi) because DurableChildId is body-level identity.
  std::unordered_map<
      uint32_t, std::unordered_map<
                    mir::DurableChildId, uint32_t, mir::DurableChildIdHash>>
      durable_children_of;

  auto WalkUp(uint32_t oi, uint32_t count) const -> uint32_t;
  auto ResolveChildObject(uint32_t parent_oi, SymbolId child_instance_sym) const
      -> uint32_t;
  auto ResolveChildByDurableId(
      uint32_t parent_body_group, const mir::DurableChildId& child_id) const
      -> uint32_t;
};

auto BuildBoundHierarchyIndex(
    const mir::ConstructionInput& construction,
    const std::unordered_map<uint32_t, std::vector<ChildSiteInfo>>&
        parent_to_children,
    const std::unordered_map<uint32_t, uint32_t>& body_to_representative,
    const std::unordered_map<uint32_t, mir::DurableChildId>&
        oi_to_durable_child) -> BoundHierarchyIndex;

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

// Build per-instance external-ref resolved global slot tables.
// For each realized object, walks each external ref recipe from that
// object's actual position in the hierarchy and computes the final
// design-global slot. Results stored in construction.instance_ext_ref_slots.
void BuildPerInstanceExternalRefSlotTables(
    const mir::Design& design, mir::ConstructionInput& construction,
    const BoundHierarchyIndex& topo);

// Populate external_refs[i].target.path with final DescendantPathStep entries
// by resolving topology-walked child object indices to DurableChildIds.
// Uses oi_to_durable_child (topology identity -> canonical identity), NOT
// debug_instance_sym. After this, target.path is authoritative and
// provisionals_by_body should not be read for path navigation.
// Requires: child_sites already propagated into bodies, target_slot filled.
// Hard-fails if any body has external_refs without matching provisionals.
void CanonicalizeExternalRefPaths(
    mir::Design& design,
    const std::unordered_map<uint32_t, std::vector<ProvisionalNonLocalTarget>>&
        provisionals_by_body,
    const BoundHierarchyIndex& topo,
    const std::unordered_map<uint32_t, mir::DurableChildId>&
        oi_to_durable_child);

// Walk a finalized NonLocalTargetRecipe path to find the target object index.
// Uses the canonical DescendantPathStep entries (DurableChildId) to resolve
// through the topology. Requires CanonicalizeExternalRefPaths to have run.
auto WalkCanonicalPath(
    const mir::NonLocalTargetRecipe& recipe, uint32_t current_oi,
    const BoundHierarchyIndex& topo, const mir::ConstructionInput& construction)
    -> uint32_t;

// Check whether a ConnectionRecipe is in the fully-bindable subset:
// source is kLocalSlot and trigger is slot-based (kLocalSlot or kChildSlot).
// Recipes with kExternalRef or kFunction source/trigger are not yet supported.
auto IsFullyBindableRecipe(const mir::ConnectionRecipe& recipe) -> bool;

// Resolve a fully-bindable ConnectionRecipe against the construction topology.
// Requires: IsFullyBindableRecipe(recipe) == true (caller must filter).
// Resolves all abstract locations to concrete BoundEndpoints and preserves
// the recipe's trigger edge semantics. The binder does not derive or
// override any semantic fields from the recipe.
auto BindConnectionRecipe(
    const mir::ConnectionRecipe& recipe, uint32_t recipe_index,
    const mir::ModuleBody& parent_body, common::ObjectIndex parent_object_index,
    const BoundHierarchyIndex& topo, const mir::ConstructionInput& construction)
    -> mir::BoundConnection;

}  // namespace lyra::lowering::hir_to_mir
