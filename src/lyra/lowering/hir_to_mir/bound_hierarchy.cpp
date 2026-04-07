#include "lyra/lowering/hir_to_mir/bound_hierarchy.hpp"

#include <cstdint>
#include <format>
#include <optional>
#include <span>
#include <unordered_map>
#include <vector>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/local_slot_id.hpp"
#include "lyra/common/symbol_types.hpp"
#include "lyra/hir/module.hpp"
#include "lyra/lowering/hir_to_mir/context.hpp"
#include "lyra/lowering/hir_to_mir/design_internal.hpp"
#include "lyra/mir/construction_input.hpp"
#include "lyra/mir/design.hpp"
#include "lyra/mir/place.hpp"

namespace lyra::lowering::hir_to_mir {

auto BoundHierarchyIndex::WalkUp(uint32_t oi, uint32_t count) const
    -> uint32_t {
  uint32_t current = oi;
  for (uint32_t i = 0; i < count; ++i) {
    if (current >= parent_of.size() || parent_of[current] == UINT32_MAX) {
      throw common::InternalError(
          "BoundHierarchyIndex::WalkUp",
          std::format("no parent at step {} from oi {}", i, oi));
    }
    current = parent_of[current];
  }
  return current;
}

auto BoundHierarchyIndex::ResolveChildObject(
    uint32_t parent_oi, SymbolId child_instance_sym) const -> uint32_t {
  auto parent_it = children_of.find(parent_oi);
  if (parent_it == children_of.end()) {
    throw common::InternalError(
        "BoundHierarchyIndex::ResolveChildObject",
        std::format("parent oi {} has no children", parent_oi));
  }
  auto child_it = parent_it->second.find(child_instance_sym);
  if (child_it == parent_it->second.end()) {
    throw common::InternalError(
        "BoundHierarchyIndex::ResolveChildObject",
        std::format(
            "child sym {} not found under parent oi {}",
            child_instance_sym.value, parent_oi));
  }
  return child_it->second;
}

auto BuildBoundHierarchyIndex(
    const mir::ConstructionInput& construction,
    const std::unordered_map<uint32_t, std::vector<ChildSiteInfo>>&
        parent_to_children,
    const std::unordered_map<uint32_t, uint32_t>& body_to_representative)
    -> BoundHierarchyIndex {
  BoundHierarchyIndex idx;
  idx.parent_of.assign(construction.objects.size(), UINT32_MAX);
  idx.rep_object_for_body = body_to_representative;

  for (const auto& [parent_idx, children] : parent_to_children) {
    for (const auto& child : children) {
      idx.parent_of[child.child_module_index] = parent_idx;
      idx.children_of[parent_idx][child.child_instance_sym] =
          child.child_module_index;
    }
  }
  return idx;
}

auto ResolveParentSourceToLocalSlot(
    SymbolId sym, const std::vector<BodyLocalSlotEntry>& body_slots)
    -> std::optional<common::LocalSlotId> {
  for (const auto& entry : body_slots) {
    if (entry.sym == sym) return entry.local_slot;
  }
  return std::nullopt;
}

auto CanonicalizeSymToRepresentative(
    SymbolId instance_sym, const hir::Module& instance_mod,
    const hir::Module& representative_mod) -> std::optional<SymbolId> {
  // If same module (representative), identity mapping.
  if (instance_mod.symbol == representative_mod.symbol) return instance_sym;
  // Search variables.
  for (size_t i = 0; i < instance_mod.variables.size(); ++i) {
    if (instance_mod.variables[i] == instance_sym &&
        i < representative_mod.variables.size()) {
      return representative_mod.variables[i];
    }
  }
  // Search nets.
  for (size_t i = 0; i < instance_mod.nets.size(); ++i) {
    if (instance_mod.nets[i] == instance_sym &&
        i < representative_mod.nets.size()) {
      return representative_mod.nets[i];
    }
  }
  // Search param_slots.
  for (size_t i = 0; i < instance_mod.param_slots.size(); ++i) {
    if (instance_mod.param_slots[i] == instance_sym &&
        i < representative_mod.param_slots.size()) {
      return representative_mod.param_slots[i];
    }
  }
  return std::nullopt;
}

auto CanonicalizeExternalRefTargetSym(
    SymbolId target_sym, uint32_t target_oi, uint32_t rep_oi,
    std::span<const hir::Module* const> hir_modules) -> SymbolId {
  if (target_oi == rep_oi) return target_sym;
  auto result = CanonicalizeSymToRepresentative(
      target_sym, *hir_modules[target_oi], *hir_modules[rep_oi]);
  if (!result) {
    throw common::InternalError(
        "CanonicalizeExternalRefTargetSym",
        std::format(
            "cannot canonicalize sym {} from oi {} to rep oi {}",
            target_sym.value, target_oi, rep_oi));
  }
  return *result;
}

auto WalkProvisionalPath(
    const ProvisionalNonLocalTarget& prov, uint32_t current_oi,
    const BoundHierarchyIndex& topo) -> uint32_t {
  uint32_t oi = topo.WalkUp(current_oi, prov.upward_count);
  for (const auto& step : prov.path) {
    switch (step.kind) {
      case ProvisionalPathStepKind::kChildInstance:
        oi = topo.ResolveChildObject(oi, step.sym);
        break;
      case ProvisionalPathStepKind::kGenerateScope:
        // Generate scopes are within the same object. No object traversal.
        break;
    }
  }
  return oi;
}

void FinalizeExternalRefTargetSlots(
    mir::Design& design,
    const std::unordered_map<uint32_t, std::vector<ProvisionalNonLocalTarget>>&
        provisionals_by_body,
    const std::unordered_map<uint32_t, std::vector<BodyLocalSlotEntry>>&
        body_local_slots_by_body,
    const BoundHierarchyIndex& topo, const mir::ConstructionInput& construction,
    std::span<const hir::Module* const> hir_modules) {
  for (uint32_t body_idx = 0; body_idx < design.module_bodies.size();
       ++body_idx) {
    auto& body = design.module_bodies[body_idx];
    if (body.external_refs.empty()) continue;

    auto prov_it = provisionals_by_body.find(body_idx);
    if (prov_it == provisionals_by_body.end()) {
      throw common::InternalError(
          "FinalizeExternalRefTargetSlots",
          std::format(
              "body {} has external_refs but no provisionals", body_idx));
    }
    const auto& provisionals = prov_it->second;
    if (provisionals.size() != body.external_refs.size()) {
      throw common::InternalError(
          "FinalizeExternalRefTargetSlots",
          std::format(
              "body {} provisionals ({}) != external_refs ({})", body_idx,
              provisionals.size(), body.external_refs.size()));
    }

    uint32_t rep_oi = topo.rep_object_for_body.at(body_idx);

    for (size_t i = 0; i < body.external_refs.size(); ++i) {
      const auto& prov = provisionals[i];
      uint32_t target_oi = WalkProvisionalPath(prov, rep_oi, topo);
      uint32_t target_body_group = construction.objects[target_oi].body_group;
      uint32_t target_rep_oi = topo.rep_object_for_body.at(target_body_group);

      SymbolId canonical_sym = CanonicalizeExternalRefTargetSym(
          prov.target_sym, target_oi, target_rep_oi, hir_modules);

      auto slots_it = body_local_slots_by_body.find(target_body_group);
      if (slots_it == body_local_slots_by_body.end()) {
        throw common::InternalError(
            "FinalizeExternalRefTargetSlots",
            std::format(
                "no slot data for target body group {}", target_body_group));
      }
      auto slot_opt =
          ResolveParentSourceToLocalSlot(canonical_sym, slots_it->second);
      if (!slot_opt) {
        throw common::InternalError(
            "FinalizeExternalRefTargetSlots",
            std::format(
                "target sym {} (canonical {}) not found in body group {}",
                prov.target_sym.value, canonical_sym.value, target_body_group));
      }
      body.external_refs[i].target.target_slot = *slot_opt;
    }
  }
}

void BuildResolvedExternalRefBindings(
    mir::Design& design,
    const std::unordered_map<uint32_t, std::vector<ProvisionalNonLocalTarget>>&
        provisionals_by_body,
    const BoundHierarchyIndex& topo,
    const mir::ConstructionInput& construction) {
  // Count objects per body group for multi-instance guard.
  std::unordered_map<uint32_t, uint32_t> objects_per_body;
  for (const auto& obj : construction.objects) {
    ++objects_per_body[obj.body_group];
  }

  for (uint32_t body_idx = 0; body_idx < design.module_bodies.size();
       ++body_idx) {
    auto& body = design.module_bodies[body_idx];
    if (body.external_refs.empty()) continue;

    // Multi-instance guard.
    if (objects_per_body[body_idx] > 1) {
      throw common::InternalError(
          "BuildResolvedExternalRefBindings",
          std::format(
              "body group {} has {} instances but active external refs -- "
              "multi-instance spec with external refs not yet supported",
              body_idx, objects_per_body[body_idx]));
    }

    auto prov_it = provisionals_by_body.find(body_idx);
    if (prov_it == provisionals_by_body.end()) continue;
    const auto& provisionals = prov_it->second;

    uint32_t rep_oi = topo.rep_object_for_body.at(body_idx);
    auto& bindings = body.resolved_external_ref_bindings;
    bindings.reserve(body.external_refs.size());

    for (size_t i = 0; i < body.external_refs.size(); ++i) {
      uint32_t target_oi = WalkProvisionalPath(provisionals[i], rep_oi, topo);
      bindings.push_back(
          mir::ResolvedExternalRefBinding{
              .target_object = common::ObjectIndex{target_oi},
              .target_local_slot = body.external_refs[i].target.target_slot,
              .type = body.external_refs[i].type});
    }
  }
}

auto ResolvePreBindingExternalRefDesignGlobalSlot(
    mir::ExternalRefId ref_id,
    const std::vector<ProvisionalNonLocalTarget>& provisional_targets,
    const std::unordered_map<SymbolId, mir::PlaceId, SymbolIdHash>&
        cross_instance_places,
    const mir::Arena& design_arena) -> uint32_t {
  if (ref_id.value >= provisional_targets.size()) {
    throw common::InternalError(
        "ResolvePreBindingExternalRefDesignGlobalSlot",
        std::format(
            "external ref {} out of range (provisional size {})", ref_id.value,
            provisional_targets.size()));
  }
  const auto& prov = provisional_targets[ref_id.value];
  auto ci_it = cross_instance_places.find(prov.target_sym);
  if (ci_it == cross_instance_places.end()) {
    throw common::InternalError(
        "ResolvePreBindingExternalRefDesignGlobalSlot",
        std::format(
            "target sym {} not found in cross_instance_places",
            prov.target_sym.value));
  }
  const auto& place = design_arena[ci_it->second];
  if (place.root.kind != mir::PlaceRoot::Kind::kDesignGlobal) {
    throw common::InternalError(
        "ResolvePreBindingExternalRefDesignGlobalSlot",
        std::format(
            "resolved place for sym {} is not kDesignGlobal",
            prov.target_sym.value));
  }
  return static_cast<uint32_t>(place.root.id);
}

}  // namespace lyra::lowering::hir_to_mir
