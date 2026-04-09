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
#include "lyra/mir/connection_endpoint.hpp"
#include "lyra/mir/connection_recipe.hpp"
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

auto BoundHierarchyIndex::ResolveChildByDurableId(
    uint32_t parent_body_group, const mir::DurableChildId& child_id) const
    -> uint32_t {
  auto parent_it = durable_children_of.find(parent_body_group);
  if (parent_it == durable_children_of.end()) {
    throw common::InternalError(
        "BoundHierarchyIndex::ResolveChildByDurableId",
        std::format(
            "parent body group {} has no durable children", parent_body_group));
  }
  auto child_it = parent_it->second.find(child_id);
  if (child_it == parent_it->second.end()) {
    throw common::InternalError(
        "BoundHierarchyIndex::ResolveChildByDurableId",
        std::format(
            "DurableChildId(ordinal={}) not found under parent body group {}",
            child_id.child_ordinal, parent_body_group));
  }
  return child_it->second;
}

auto BuildBoundHierarchyIndex(
    const mir::ConstructionInput& construction,
    const std::unordered_map<uint32_t, std::vector<ChildSiteInfo>>&
        parent_to_children,
    const std::unordered_map<uint32_t, uint32_t>& body_to_representative,
    const std::unordered_map<uint32_t, mir::DurableChildId>&
        oi_to_durable_child) -> BoundHierarchyIndex {
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

  // Build durable_children_of: body_group -> {DurableChildId -> child_oi}.
  // Groups representative children by their parent's body_group.
  for (const auto& [child_oi, durable_id] : oi_to_durable_child) {
    uint32_t parent_oi = idx.parent_of[child_oi];
    if (parent_oi == UINT32_MAX) continue;
    uint32_t parent_bg = construction.objects[parent_oi].body_group;
    idx.durable_children_of[parent_bg][durable_id] = child_oi;
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

void EnforceExternalRefSingleInstanceGuard(
    const mir::Design& design, const mir::ConstructionInput& construction) {
  std::unordered_map<uint32_t, uint32_t> objects_per_body;
  for (const auto& obj : construction.objects) {
    ++objects_per_body[obj.body_group];
  }
  for (uint32_t body_idx = 0; body_idx < design.module_bodies.size();
       ++body_idx) {
    if (design.module_bodies[body_idx].external_refs.empty()) continue;
    if (objects_per_body[body_idx] > 1) {
      throw common::InternalError(
          "EnforceExternalRefSingleInstanceGuard",
          std::format(
              "body group {} has {} instances but active external refs -- "
              "multi-instance spec with external refs not yet supported",
              body_idx, objects_per_body[body_idx]));
    }
  }
}

void CanonicalizeExternalRefPaths(
    mir::Design& design,
    const std::unordered_map<uint32_t, std::vector<ProvisionalNonLocalTarget>>&
        provisionals_by_body,
    const BoundHierarchyIndex& topo, const mir::ConstructionInput& construction,
    const std::unordered_map<uint32_t, mir::DurableChildId>&
        oi_to_durable_child) {
  for (uint32_t body_idx = 0; body_idx < design.module_bodies.size();
       ++body_idx) {
    auto& body = design.module_bodies[body_idx];
    if (body.external_refs.empty()) continue;

    auto prov_it = provisionals_by_body.find(body_idx);
    if (prov_it == provisionals_by_body.end()) {
      throw common::InternalError(
          "CanonicalizeExternalRefPaths",
          std::format(
              "body {} has external_refs but no provisionals", body_idx));
    }
    const auto& provisionals = prov_it->second;
    if (provisionals.size() != body.external_refs.size()) {
      throw common::InternalError(
          "CanonicalizeExternalRefPaths",
          std::format(
              "body {} provisionals ({}) != external_refs ({})", body_idx,
              provisionals.size(), body.external_refs.size()));
    }

    uint32_t rep_oi = topo.rep_object_for_body.at(body_idx);

    for (size_t i = 0; i < body.external_refs.size(); ++i) {
      const auto& prov = provisionals[i];
      auto& recipe = body.external_refs[i].target;

      // Walk the path, resolving child_oi through topology then mapping
      // to DurableChildId via oi_to_durable_child. No debug_instance_sym.
      uint32_t oi = topo.WalkUp(rep_oi, prov.upward_count);
      std::vector<mir::DescendantPathStep> path;

      for (const auto& step : prov.path) {
        switch (step.kind) {
          case ProvisionalPathStepKind::kChildInstance: {
            uint32_t child_oi = topo.ResolveChildObject(oi, step.sym);
            auto it = oi_to_durable_child.find(child_oi);
            if (it == oi_to_durable_child.end()) {
              throw common::InternalError(
                  "CanonicalizeExternalRefPaths",
                  std::format(
                      "child oi {} has no DurableChildId mapping "
                      "(parent oi {})",
                      child_oi, oi));
            }
            path.push_back(mir::DescendantPathStep{.child = it->second});
            oi = child_oi;
            break;
          }
          case ProvisionalPathStepKind::kGenerateScope:
            // Generate scopes stay within the same object. The scope
            // context is already encoded in the child's DurableChildId.coord.
            break;
        }
      }

      recipe.path = std::move(path);
    }
  }
}

auto WalkCanonicalPath(
    const mir::NonLocalTargetRecipe& recipe, uint32_t current_oi,
    const BoundHierarchyIndex& topo, const mir::ConstructionInput& construction)
    -> uint32_t {
  uint32_t oi = topo.WalkUp(current_oi, recipe.upward_count);
  for (const auto& step : recipe.path) {
    uint32_t parent_bg = construction.objects[oi].body_group;
    oi = topo.ResolveChildByDurableId(parent_bg, step.child);
  }
  return oi;
}

void BuildResolvedExternalRefBindings(
    mir::Design& design, const BoundHierarchyIndex& topo,
    const mir::ConstructionInput& construction) {
  // Guard already enforced by EnforceExternalRefSingleInstanceGuard
  // before this pass runs.
  for (uint32_t body_idx = 0; body_idx < design.module_bodies.size();
       ++body_idx) {
    auto& body = design.module_bodies[body_idx];
    if (body.external_refs.empty()) continue;

    uint32_t rep_oi = topo.rep_object_for_body.at(body_idx);
    auto& bindings = body.resolved_external_ref_bindings;
    bindings.reserve(body.external_refs.size());

    for (size_t i = 0; i < body.external_refs.size(); ++i) {
      const auto& recipe = body.external_refs[i].target;
      uint32_t target_oi =
          WalkCanonicalPath(recipe, rep_oi, topo, construction);
      bindings.push_back(
          mir::ResolvedExternalRefBinding{
              .target_object = common::ObjectIndex{target_oi},
              .target_local_slot = recipe.target_slot,
              .type = body.external_refs[i].type});
    }
  }
}

auto IsFullyBindableRecipe(const mir::ConnectionRecipe& recipe) -> bool {
  // Fully bindable: kLocalSlot source AND slot-based trigger
  // (kLocalSlot for parent-local, kChildSlot for child-local).
  // The binder resolves both to concrete BoundEndpoints and preserves
  // the recipe's trigger edge semantics. Recipes with kExternalRef or
  // kFunction source/trigger are not yet supported.
  return recipe.source.kind == mir::ConnectionSourceRecipe::Kind::kLocalSlot &&
         (recipe.trigger.kind == mir::TriggerRecipe::Kind::kLocalSlot ||
          recipe.trigger.kind == mir::TriggerRecipe::Kind::kChildSlot);
}

auto BindConnectionRecipe(
    const mir::ConnectionRecipe& recipe, uint32_t recipe_index,
    const mir::ModuleBody& parent_body, common::ObjectIndex parent_object_index,
    const BoundHierarchyIndex& topo, const mir::ConstructionInput& construction)
    -> mir::BoundConnection {
  if (!IsFullyBindableRecipe(recipe)) {
    throw common::InternalError(
        "BindConnectionRecipe",
        std::format(
            "recipe {} is not fully bindable (source kind={}, trigger "
            "kind={}, connection kind={})",
            recipe_index, static_cast<uint8_t>(recipe.source.kind),
            static_cast<uint8_t>(recipe.trigger.kind),
            static_cast<uint8_t>(recipe.kind)));
  }
  // Resolve child site to concrete object.
  if (recipe.child_site.value >= parent_body.child_sites.size()) {
    throw common::InternalError(
        "BindConnectionRecipe",
        std::format(
            "child_site {} out of range ({} sites)", recipe.child_site.value,
            parent_body.child_sites.size()));
  }
  const auto& site = parent_body.child_sites[recipe.child_site.value];
  uint32_t parent_bg =
      construction.objects[parent_object_index.value].body_group;
  uint32_t child_oi = topo.ResolveChildByDurableId(parent_bg, site.id);

  common::ObjectIndex child_obj{child_oi};

  // Resolve source: always parent-side. For kDriveParentToChild this is
  // the data source; for kDriveChildToParent this is the parent destination.
  mir::BoundEndpoint parent_ep{
      .object_index = parent_object_index,
      .local_slot = recipe.source.local_slot};
  mir::BoundEndpoint child_ep{
      .object_index = child_obj, .local_slot = recipe.child_slot};

  // Resolve trigger from recipe. The trigger kind determines which
  // object owns the slot: kLocalSlot -> parent, kChildSlot -> child.
  mir::BoundEndpoint trigger_ep =
      (recipe.trigger.kind == mir::TriggerRecipe::Kind::kLocalSlot)
          ? mir::BoundEndpoint{.object_index = parent_object_index,
                               .local_slot = recipe.trigger.local_slot}
          : mir::BoundEndpoint{.object_index = child_obj,
                               .local_slot = recipe.trigger.local_slot};

  return mir::BoundConnection{
      .recipe_index = recipe_index,
      .kind = recipe.kind,
      .parent_object_index = parent_object_index,
      .child_target = child_ep,
      .parent_source = parent_ep,
      .trigger = trigger_ep,
      .trigger_edge = recipe.trigger.edge,
      .result_type = recipe.result_type};
}

}  // namespace lyra::lowering::hir_to_mir
