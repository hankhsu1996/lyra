#include "lyra/llvm_backend/behavioral_trigger_contracts.hpp"

#include <cstdint>
#include <format>
#include <optional>
#include <span>
#include <unordered_map>
#include <variant>
#include <vector>

#include "lyra/common/internal_error.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/basic_block.hpp"
#include "lyra/mir/construction_input.hpp"
#include "lyra/mir/module.hpp"
#include "lyra/mir/terminator.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

// Build per-body behavioral trigger bitmaps. Positional: parallel to
// layout.body_realization_infos. Each bitmap marks body-local slots
// that appear as behavioral wait triggers in non-final processes.
auto BuildBodyBehavioralDirtyTriggerBitmaps(const Layout& layout)
    -> std::vector<std::vector<bool>> {
  std::vector<std::vector<bool>> result(layout.body_realization_infos.size());

  for (size_t gi = 0; gi < layout.body_realization_infos.size(); ++gi) {
    const auto& info = layout.body_realization_infos[gi];
    const auto& body = *info.body;
    std::vector<bool> bitmap(info.slot_count, false);

    auto mark_local = [&](uint32_t local_slot, const char* source) {
      if (local_slot >= info.slot_count) {
        throw common::InternalError(
            "BuildBodyBehavioralDirtyTriggerBitmaps",
            std::format(
                "{} slot {} out of range for body group {} "
                "(slot_count={})",
                source, local_slot, gi, info.slot_count));
      }
      bitmap[local_slot] = true;
    };

    for (const auto& proc_id : body.processes) {
      const auto& process = body.arena[proc_id];
      if (process.kind == mir::ProcessKind::kFinal) continue;

      for (const auto& block : process.blocks) {
        const auto* wait = std::get_if<mir::Wait>(&block.terminator.data);
        if (wait == nullptr) continue;
        for (const auto& trigger : wait->triggers) {
          if (trigger.unresolved_external_ref.has_value()) continue;
          if (trigger.signal.scope == mir::SignalRef::Scope::kModuleLocal) {
            mark_local(
                static_cast<uint32_t>(trigger.signal.id),
                "module-local trigger");
          }
          if (trigger.late_bound.has_value()) {
            for (const auto& dep : trigger.late_bound->dep_slots) {
              if (dep.scope == mir::ScopedSlotRef::Scope::kModuleLocal) {
                mark_local(static_cast<uint32_t>(dep.id), "late-bound dep");
              }
            }
          }
        }
      }
    }

    result[gi] = std::move(bitmap);
  }

  return result;
}

// Mark body-local slots that are targets of cross-body ext-ref behavioral
// triggers. For each body with ext-ref wait triggers, resolve each triggered
// ext-ref binding to the target body and mark the target local slot in
// that body's bitmap. This is body-local identity, not design-global.
void MarkExtRefTriggerTargetsInBodyBitmaps(
    const Layout& layout, const mir::ConstructionInput& construction,
    std::vector<std::vector<bool>>& bitmaps) {
  // Step 1: per body group, collect which ext-ref indices appear in triggers.
  std::unordered_map<uint32_t, std::vector<uint32_t>> trigger_refs_by_body;
  for (size_t gi = 0; gi < layout.body_realization_infos.size(); ++gi) {
    auto body_group_id = static_cast<uint32_t>(gi);
    if (trigger_refs_by_body.contains(body_group_id)) continue;
    const auto& body = *layout.body_realization_infos[gi].body;
    std::vector<uint32_t> refs;
    for (const auto& proc_id : body.processes) {
      const auto& process = body.arena[proc_id];
      if (process.kind == mir::ProcessKind::kFinal) continue;
      for (const auto& block : process.blocks) {
        const auto* wait = std::get_if<mir::Wait>(&block.terminator.data);
        if (wait == nullptr) continue;
        for (const auto& trigger : wait->triggers) {
          if (trigger.unresolved_external_ref.has_value()) {
            refs.push_back(trigger.unresolved_external_ref->value);
          }
        }
      }
    }
    if (!refs.empty()) {
      trigger_refs_by_body[body_group_id] = std::move(refs);
    }
  }
  if (trigger_refs_by_body.empty()) return;

  // Step 2: for each instance with ext-ref triggers, resolve to target
  // body and mark the target local slot in that body's bitmap.
  const auto& bindings = construction.instance_ext_ref_bindings;
  for (uint32_t oi = 0; oi < construction.objects.size(); ++oi) {
    uint32_t body_group = construction.objects[oi].body_group;
    auto it = trigger_refs_by_body.find(body_group);
    if (it == trigger_refs_by_body.end()) continue;
    if (oi >= bindings.size() || bindings[oi].empty()) continue;

    for (uint32_t ref_idx : it->second) {
      if (ref_idx >= bindings[oi].size()) continue;
      const auto& binding = bindings[oi][ref_idx];
      // Find the target body from the target instance's object record.
      uint32_t target_oi = binding.target_instance_id;
      if (target_oi >= construction.objects.size()) continue;
      uint32_t target_body = construction.objects[target_oi].body_group;
      uint32_t target_local = binding.target_local_signal.value;

      if (target_body < bitmaps.size() &&
          target_local < bitmaps[target_body].size()) {
        bitmaps[target_body][target_local] = true;
      }
    }
  }
}

void PopulateBodyBitmaps(
    const std::vector<std::vector<bool>>& bitmaps, Layout& layout) {
  for (size_t gi = 0; gi < layout.body_realization_infos.size(); ++gi) {
    auto& info = layout.body_realization_infos[gi];
    const auto& bitmap = bitmaps[gi];
    if (bitmap.size() != info.slot_count) {
      throw common::InternalError(
          "PopulateBehavioralTriggerContracts",
          std::format(
              "behavioral trigger bitmap size mismatch for body "
              "group {}: got {}, expected {}",
              gi, bitmap.size(), info.slot_count));
    }
    info.slot_has_behavioral_trigger = bitmap;
  }
}

}  // namespace

void PopulateBehavioralTriggerContracts(
    std::span<const LayoutModulePlan> module_plans, const mir::Design& design,
    const mir::Arena& design_arena, const mir::ConstructionInput& construction,
    Layout& layout) {
  auto body_bitmaps = BuildBodyBehavioralDirtyTriggerBitmaps(layout);
  // Mark target body-local slots that have cross-body ext-ref subscribers.
  MarkExtRefTriggerTargetsInBodyBitmaps(layout, construction, body_bitmaps);
  PopulateBodyBitmaps(body_bitmaps, layout);

  // Compute per-body cross-body behavioral trigger bitmaps from trigger
  // facts and canonical ownership. A slot is cross-body iff a process in
  // a DIFFERENT body (or an init process) has a behavioral trigger that
  // resolves to that body-local slot, AND the owning body does not
  // already have a body-local trigger on the same slot.
  std::unordered_map<const mir::ModuleBody*, size_t> body_to_group;
  for (size_t gi = 0; gi < layout.body_realization_infos.size(); ++gi) {
    body_to_group[layout.body_realization_infos[gi].body] = gi;
  }
  for (auto& info : layout.body_realization_infos) {
    info.slot_has_cross_body_behavioral_trigger.assign(info.slot_count, false);
  }

  auto resolve_design_global_to_body_local = [&](uint32_t design_global_slot)
      -> std::optional<std::pair<size_t, uint32_t>> {
    for (const auto& plan : module_plans) {
      if (design_global_slot >= plan.design_state_base_slot &&
          design_global_slot < plan.design_state_base_slot + plan.slot_count) {
        auto it = body_to_group.find(plan.body);
        if (it == body_to_group.end()) return std::nullopt;
        return std::pair{
            it->second, design_global_slot - plan.design_state_base_slot};
      }
    }
    return std::nullopt;
  };

  auto mark_cross_body = [&](size_t owner_group, uint32_t local_slot) {
    auto& owner = layout.body_realization_infos[owner_group];
    if (owner.slot_has_behavioral_trigger[local_slot]) return;
    owner.slot_has_cross_body_behavioral_trigger[local_slot] = true;
  };

  for (const auto& proc_id : design.init_processes) {
    const auto& process = design_arena[proc_id];
    for (const auto& block : process.blocks) {
      const auto* wait = std::get_if<mir::Wait>(&block.terminator.data);
      if (wait == nullptr) continue;
      for (const auto& trigger : wait->triggers) {
        if (trigger.signal.scope == mir::SignalRef::Scope::kDesignGlobal) {
          auto resolved = resolve_design_global_to_body_local(
              static_cast<uint32_t>(trigger.signal.id));
          if (resolved) {
            mark_cross_body(resolved->first, resolved->second);
          }
        }
      }
    }
  }

  // Body processes: design-global triggers.
  // Mark cross-body only when source body differs from owner body.
  for (size_t source_gi = 0; source_gi < layout.body_realization_infos.size();
       ++source_gi) {
    const auto& source_info = layout.body_realization_infos[source_gi];
    const auto& body = *source_info.body;
    for (const auto& proc_id : body.processes) {
      const auto& process = body.arena[proc_id];
      if (process.kind == mir::ProcessKind::kFinal) continue;
      for (const auto& block : process.blocks) {
        const auto* wait = std::get_if<mir::Wait>(&block.terminator.data);
        if (wait == nullptr) continue;
        for (const auto& trigger : wait->triggers) {
          if (trigger.signal.scope == mir::SignalRef::Scope::kDesignGlobal) {
            auto resolved = resolve_design_global_to_body_local(
                static_cast<uint32_t>(trigger.signal.id));
            if (!resolved) continue;
            if (resolved->first == source_gi) continue;
            mark_cross_body(resolved->first, resolved->second);
          }
        }
      }
    }
  }

  // External-ref triggers: resolve directly to (body_group, local_slot)
  // using per-instance binding records from construction input.
  const auto& bindings = construction.instance_ext_ref_bindings;
  for (uint32_t oi = 0; oi < construction.objects.size(); ++oi) {
    uint32_t source_body_group = construction.objects[oi].body_group;
    if (oi >= bindings.size() || bindings[oi].empty()) continue;
    const auto& body = *layout.body_realization_infos[source_body_group].body;

    // Collect ext-ref indices from triggers in this body's processes.
    for (const auto& proc_id : body.processes) {
      const auto& process = body.arena[proc_id];
      if (process.kind == mir::ProcessKind::kFinal) continue;
      for (const auto& block : process.blocks) {
        const auto* wait = std::get_if<mir::Wait>(&block.terminator.data);
        if (wait == nullptr) continue;
        for (const auto& trigger : wait->triggers) {
          if (!trigger.unresolved_external_ref.has_value()) continue;
          auto ref_idx = trigger.unresolved_external_ref->value;
          if (ref_idx >= bindings[oi].size()) continue;
          const auto& binding = bindings[oi][ref_idx];
          uint32_t target_oi = binding.target_instance_id;
          if (target_oi >= construction.objects.size()) continue;
          uint32_t owner_group = construction.objects[target_oi].body_group;
          if (owner_group == source_body_group) continue;
          uint32_t local_slot = binding.target_local_signal.value;
          if (owner_group < layout.body_realization_infos.size() &&
              local_slot <
                  layout.body_realization_infos[owner_group].slot_count) {
            mark_cross_body(owner_group, local_slot);
          }
        }
      }
    }
  }
}

}  // namespace lyra::lowering::mir_to_llvm
