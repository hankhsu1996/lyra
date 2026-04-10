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

struct BodyBehavioralTriggerBitmaps {
  std::unordered_map<uint32_t, std::vector<bool>> by_body_id_value;
};

auto BuildBodyBehavioralDirtyTriggerBitmaps(
    std::span<const LayoutModulePlan> module_plans,
    std::span<const std::span<const mir::ProcessId>> module_body_processes,
    const mir::Design& design) -> BodyBehavioralTriggerBitmaps {
  BodyBehavioralTriggerBitmaps result;

  for (size_t mi = 0; mi < module_plans.size(); ++mi) {
    const auto& plan = module_plans[mi];
    auto body_id_val =
        static_cast<uint32_t>(plan.body - design.module_bodies.data());
    if (result.by_body_id_value.contains(body_id_val)) continue;

    std::vector<bool> bitmap(plan.slot_count, false);
    const auto& body = *plan.body;

    for (const auto& proc_id : module_body_processes[mi]) {
      const auto& process = body.arena[proc_id];
      if (process.kind == mir::ProcessKind::kFinal) continue;

      auto mark_local = [&](uint32_t local_slot, const char* source) {
        if (local_slot >= plan.slot_count) {
          throw common::InternalError(
              "BuildBodyBehavioralDirtyTriggerBitmaps",
              std::format(
                  "{} slot {} out of range for body {} "
                  "(slot_count={})",
                  source, local_slot, body_id_val, plan.slot_count));
        }
        bitmap[local_slot] = true;
      };

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

    result.by_body_id_value[body_id_val] = std::move(bitmap);
  }

  return result;
}

// Mark body-local slots that are targets of cross-body ext-ref behavioral
// triggers. For each body with ext-ref wait triggers, resolve each triggered
// ext-ref binding to the target body and mark the target local slot in
// that body's bitmap. This is body-local identity, not design-global.
void MarkExtRefTriggerTargetsInBodyBitmaps(
    std::span<const LayoutModulePlan> module_plans,
    std::span<const std::span<const mir::ProcessId>> module_body_processes,
    const mir::Design& design, const mir::ConstructionInput& construction,
    BodyBehavioralTriggerBitmaps& bitmaps) {
  // Step 1: per body, collect which ext-ref indices appear in triggers.
  std::unordered_map<uint32_t, std::vector<uint32_t>> trigger_refs_by_body;
  for (size_t mi = 0; mi < module_plans.size(); ++mi) {
    const auto& plan = module_plans[mi];
    auto body_id =
        static_cast<uint32_t>(plan.body - design.module_bodies.data());
    if (trigger_refs_by_body.contains(body_id)) continue;
    const auto& body = *plan.body;
    std::vector<uint32_t> refs;
    for (const auto& proc_id : module_body_processes[mi]) {
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
      trigger_refs_by_body[body_id] = std::move(refs);
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

      auto& bitmap = bitmaps.by_body_id_value[target_body];
      if (bitmap.empty()) {
        bitmap.assign(construction.objects[target_oi].slot_count, false);
      }
      if (target_local < bitmap.size()) {
        bitmap[target_local] = true;
      }
    }
  }
}

auto BuildDesignGlobalBehavioralTriggerBitmap(
    std::span<const LayoutModulePlan> module_plans,
    std::span<const std::span<const mir::ProcessId>> module_body_processes,
    const mir::Design& design, const mir::Arena& design_arena,
    const DesignLayout& design_layout,
    const BodyBehavioralTriggerBitmaps& body_bitmaps) -> std::vector<bool> {
  auto num_slots = design_layout.slots.size();
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
  for (size_t mi = 0; mi < module_plans.size(); ++mi) {
    const auto& plan = module_plans[mi];
    const auto& body = *plan.body;
    for (const auto& proc_id : module_body_processes[mi]) {
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
  for (const auto& plan : module_plans) {
    auto plan_body_id_val =
        static_cast<uint32_t>(plan.body - design.module_bodies.data());
    auto it = body_bitmaps.by_body_id_value.find(plan_body_id_val);
    if (it == body_bitmaps.by_body_id_value.end()) continue;
    const auto& body_bitmap = it->second;
    for (uint32_t local_slot = 0; local_slot < body_bitmap.size();
         ++local_slot) {
      if (!body_bitmap[local_slot]) continue;
      uint32_t design_slot_row = plan.design_state_base_slot + local_slot;
      if (design_slot_row >= design_layout.slots.size()) {
        throw common::InternalError(
            "BuildDesignGlobalBehavioralTriggerBitmap",
            std::format(
                "body {} local_slot {} maps to design_slot_row {} "
                "out of range ({} slots)",
                plan_body_id_val, local_slot, design_slot_row,
                design_layout.slots.size()));
      }
      auto canonical_slot = design_layout.slots[design_slot_row];
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
    const BodyBehavioralTriggerBitmaps& bitmaps, Layout& layout) {
  for (auto& info : layout.body_realization_infos) {
    auto it = bitmaps.by_body_id_value.find(info.body_id.value);
    if (it != bitmaps.by_body_id_value.end()) {
      if (it->second.size() != info.slot_count) {
        throw common::InternalError(
            "PopulateBodyBehavioralTriggerContracts",
            std::format(
                "behavioral trigger bitmap size mismatch for body "
                "{}: got {}, expected {}",
                info.body_id.value, it->second.size(), info.slot_count));
      }
      info.slot_has_behavioral_trigger = it->second;
    } else {
      info.slot_has_behavioral_trigger.assign(info.slot_count, false);
    }
  }
}

}  // namespace

void PopulateBehavioralTriggerContracts(
    std::span<const LayoutModulePlan> module_plans,
    std::span<const std::span<const mir::ProcessId>> module_body_processes,
    const mir::Design& design, const mir::Arena& design_arena,
    const mir::ConstructionInput& construction, Layout& layout) {
  auto body_bitmaps = BuildBodyBehavioralDirtyTriggerBitmaps(
      module_plans, module_body_processes, design);
  // Mark target body-local slots that have cross-body ext-ref subscribers.
  MarkExtRefTriggerTargetsInBodyBitmaps(
      module_plans, module_body_processes, design, construction, body_bitmaps);
  PopulateBodyBitmaps(body_bitmaps, layout);
  layout.slot_has_design_behavioral_trigger =
      BuildDesignGlobalBehavioralTriggerBitmap(
          module_plans, module_body_processes, design, design_arena,
          layout.design, body_bitmaps);
}

}  // namespace lyra::lowering::mir_to_llvm
