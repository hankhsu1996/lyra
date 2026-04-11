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
#include "lyra/mir/module.hpp"
#include "lyra/mir/terminator.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

struct BodyBehavioralTriggerBitmaps {
  std::unordered_map<uint32_t, std::vector<bool>> by_body_id_value;
};

auto BuildBodyBehavioralDirtyTriggerBitmaps(
    std::span<const LayoutModulePlan> module_plans, const mir::Design& design)
    -> BodyBehavioralTriggerBitmaps {
  BodyBehavioralTriggerBitmaps result;

  for (const auto& plan : module_plans) {
    auto body_id_val =
        static_cast<uint32_t>(plan.body - design.module_bodies.data());
    if (result.by_body_id_value.contains(body_id_val)) continue;

    std::vector<bool> bitmap(plan.slot_count, false);
    const auto& body = *plan.body;

    for (const auto& proc_id : plan.body_processes) {
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

auto BuildDesignGlobalBehavioralTriggerBitmap(
    std::span<const LayoutModulePlan> module_plans, const mir::Design& design,
    const mir::Arena& design_arena, const DesignLayout& design_layout,
    const BodyBehavioralTriggerBitmaps& body_bitmaps) -> std::vector<bool> {
  auto num_slots = design_layout.slots.size();
  std::vector<bool> bitmap(num_slots, false);

  auto mark_global_slot = [&](common::SlotId trigger_slot) {
    if (trigger_slot.value >= num_slots) {
      throw common::InternalError(
          "BuildDesignGlobalBehavioralTriggerBitmap",
          std::format(
              "design-global trigger slot {} out of "
              "range ({} slots)",
              trigger_slot.value, num_slots));
    }
    bitmap[trigger_slot.value] = true;
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
        mark_global_slot(
            common::SlotId{static_cast<uint32_t>(trigger.signal.id)});
      }
    }
  }

  // Body processes with kDesignGlobal or unresolved external ref triggers.
  for (const auto& plan : module_plans) {
    const auto& body = *plan.body;
    for (const auto& proc_id : plan.body_processes) {
      const auto& process = body.arena[proc_id];
      if (process.kind == mir::ProcessKind::kFinal) continue;
      for (const auto& block : process.blocks) {
        const auto* wait = std::get_if<mir::Wait>(&block.terminator.data);
        if (wait == nullptr) continue;
        for (const auto& trigger : wait->triggers) {
          if (trigger.unresolved_external_ref.has_value()) {
            // External-ref signal identity is per-instance. Skip in the
            // compile-time behavioral dirty bitmap. Per-instance trigger
            // resolution is a separate architectural change.
            continue;
          }
          if (trigger.signal.scope != mir::SignalRef::Scope::kDesignGlobal) {
            continue;
          }
          mark_global_slot(
              common::SlotId{static_cast<uint32_t>(trigger.signal.id)});
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
      mark_global_slot(canonical_slot);
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
    std::span<const LayoutModulePlan> module_plans, const mir::Design& design,
    const mir::Arena& design_arena, Layout& layout) {
  auto body_bitmaps =
      BuildBodyBehavioralDirtyTriggerBitmaps(module_plans, design);
  PopulateBodyBitmaps(body_bitmaps, layout);
  layout.slot_has_design_behavioral_trigger =
      BuildDesignGlobalBehavioralTriggerBitmap(
          module_plans, design, design_arena, layout.design, body_bitmaps);
}

}  // namespace lyra::lowering::mir_to_llvm
