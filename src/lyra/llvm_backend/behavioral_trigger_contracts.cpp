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
    const Layout& layout,
    std::span<const LayoutModulePlan> module_plans,
    const mir::Design& design, const mir::ConstructionInput& construction,
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

auto BuildDesignGlobalBehavioralTriggerBitmap(
    const Layout& layout, std::span<const LayoutModulePlan> module_plans,
    const mir::Design& design, const mir::Arena& design_arena,
    const std::vector<std::vector<bool>>& body_bitmaps) -> std::vector<bool> {
  auto num_slots = layout.design.slots.size();
  std::vector<bool> bitmap(num_slots, false);

  auto mark_global_slot = [&](uint32_t slot) {
    if (slot >= num_slots) {
      throw common::InternalError(
          "BuildDesignGlobalBehavioralTriggerBitmap",
          std::format(
              "design-global trigger slot {} out of "
              "range ({} slots)",
              slot, num_slots));
    }
    bitmap[slot] = true;
  };

  // Init processes: design-level, data lives in design_arena.
  for (const auto& proc_id : design.init_processes) {
    const auto& process = design_arena[proc_id];
    for (const auto& block : process.blocks) {
      const auto* wait = std::get_if<mir::Wait>(&block.terminator.data);
      if (wait == nullptr) continue;
      for (const auto& trigger : wait->triggers) {
        if (trigger.signal.scope != mir::SignalRef::Scope::kDesignGlobal) {
          throw common::InternalError(
              "BuildDesignGlobalBehavioralTriggerBitmap",
              std::format(
                  "init process {} has trigger with "
                  "non-design-global scope (id={})",
                  proc_id.value, trigger.signal.id));
        }
        mark_global_slot(static_cast<uint32_t>(trigger.signal.id));
      }
    }
  }

  // Body processes with kDesignGlobal triggers.
  // Iterate body groups (not instances): body-level scanning is the same
  // for all instances of the same body.
  for (size_t gi = 0; gi < layout.body_realization_infos.size(); ++gi) {
    const auto& info = layout.body_realization_infos[gi];
    const auto& body = *info.body;
    for (const auto& proc_id : body.processes) {
      const auto& process = body.arena[proc_id];
      if (process.kind == mir::ProcessKind::kFinal) continue;
      for (const auto& block : process.blocks) {
        const auto* wait = std::get_if<mir::Wait>(&block.terminator.data);
        if (wait == nullptr) continue;
        for (const auto& trigger : wait->triggers) {
          if (trigger.unresolved_external_ref.has_value()) continue;
          if (trigger.signal.scope != mir::SignalRef::Scope::kDesignGlobal) {
            continue;
          }
          mark_global_slot(static_cast<uint32_t>(trigger.signal.id));
        }
      }
    }
  }

  // Project body-local behavioral triggers onto design-global slots.
  // This is the only instance-level step: each instance has its own
  // design_state_base_slot, so we iterate module_plans and route
  // through body pointer to find the positional body-group bitmap.
  std::unordered_map<const mir::ModuleBody*, size_t> body_to_group;
  for (size_t gi = 0; gi < layout.body_realization_infos.size(); ++gi) {
    body_to_group[layout.body_realization_infos[gi].body] = gi;
  }
  for (const auto& plan : module_plans) {
    auto it = body_to_group.find(plan.body);
    if (it == body_to_group.end()) continue;
    const auto& body_bitmap = body_bitmaps[it->second];
    for (uint32_t local_slot = 0; local_slot < body_bitmap.size();
         ++local_slot) {
      if (!body_bitmap[local_slot]) continue;
      uint32_t design_slot_row = plan.design_state_base_slot + local_slot;
      if (design_slot_row >= layout.design.slots.size()) {
        throw common::InternalError(
            "BuildDesignGlobalBehavioralTriggerBitmap",
            std::format(
                "body group {} local_slot {} maps to design_slot_row {} "
                "out of range ({} slots)",
                it->second, local_slot, design_slot_row,
                layout.design.slots.size()));
      }
      auto canonical_slot = layout.design.slots[design_slot_row];
      if (canonical_slot.value != design_slot_row) {
        throw common::InternalError(
            "BuildDesignGlobalBehavioralTriggerBitmap",
            std::format(
                "slot identity invariant violated: "
                "design_layout.slots[{}].value={}, expected identity",
                design_slot_row, canonical_slot.value));
      }
      mark_global_slot(canonical_slot.value);
    }
  }

  return bitmap;
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
  MarkExtRefTriggerTargetsInBodyBitmaps(
      layout, module_plans, design, construction, body_bitmaps);
  PopulateBodyBitmaps(body_bitmaps, layout);
  layout.slot_has_design_behavioral_trigger =
      BuildDesignGlobalBehavioralTriggerBitmap(
          layout, module_plans, design, design_arena, body_bitmaps);

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
          if (trigger.unresolved_external_ref.has_value()) {
            auto ref_id = *trigger.unresolved_external_ref;
            if (ref_id.value >= body.resolved_external_ref_bindings.size()) {
              continue;
            }
            const auto& binding =
                body.resolved_external_ref_bindings[ref_id.value];
            if (binding.IsPackageOrGlobal()) continue;
            if (binding.target_object.value >=
                construction.objects.size()) {
              continue;
            }
            auto owner_group =
                construction.objects[binding.target_object.value].body_group;
            if (owner_group == source_gi) continue;
            auto local_slot = binding.target_local_slot.value;
            if (owner_group < layout.body_realization_infos.size() &&
                local_slot <
                    layout.body_realization_infos[owner_group].slot_count) {
              mark_cross_body(owner_group, local_slot);
            }
            continue;
          }
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
}

}  // namespace lyra::lowering::mir_to_llvm
