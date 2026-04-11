#include "lyra/llvm_backend/spec_planning.hpp"

#include <cstdint>
#include <format>
#include <string>
#include <unordered_map>
#include <variant>
#include <vector>

#include "lyra/common/internal_error.hpp"
#include "lyra/llvm_backend/process_meta_utils.hpp"
#include "lyra/lowering/origin_map_lookup.hpp"
#include "lyra/mir/call.hpp"
#include "lyra/mir/module.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

auto BodyIndex(const mir::ModuleBody* body, const mir::Design& design)
    -> uint32_t {
  return static_cast<uint32_t>(body - design.module_bodies.data());
}

auto BuildSpecCompilationUnits(const mir::Design& design)
    -> std::vector<SpecCompilationUnit> {
  std::vector<SpecCompilationUnit> units;
  std::unordered_map<const mir::ModuleBody*, size_t> body_to_unit;

  uint32_t module_idx = 0;
  for (const auto& element : design.elements) {
    const auto* mod = std::get_if<mir::Module>(&element);
    if (mod == nullptr) continue;

    SpecInstanceBinding binding{
        .module_index = ModuleIndex{module_idx},
    };

    auto [it, inserted] = body_to_unit.try_emplace(mod->body, units.size());
    if (inserted) {
      const auto& body = *mod->body;
      units.push_back(
          SpecCompilationUnit{
              .body = mod->body,
              .processes = {body.processes.begin(), body.processes.end()},
              .functions = {body.functions.begin(), body.functions.end()},
              .instances = {binding},
          });
    } else {
      units[it->second].instances.push_back(binding);
    }

    ++module_idx;
  }

  return units;
}

auto BuildModuleSchedIndices(
    uint32_t num_init_processes,
    std::span<const ScheduledProcess> scheduled_processes)
    -> std::unordered_map<uint32_t, std::vector<uint32_t>> {
  std::unordered_map<uint32_t, std::vector<uint32_t>> result;
  for (size_t i = num_init_processes; i < scheduled_processes.size(); ++i) {
    const auto& sp = scheduled_processes[i];
    if (sp.module_index.value == ModuleIndex::kNone) continue;
    result[sp.module_index.value].push_back(static_cast<uint32_t>(i));
  }
  return result;
}

auto BuildSpecCodegenViews(
    const std::vector<SpecCompilationUnit>& units, const mir::Design& design,
    const std::unordered_map<uint32_t, std::vector<uint32_t>>&
        modidx_to_sched_indices) -> std::vector<SpecCodegenView> {
  std::vector<SpecCodegenView> views(units.size());

  for (size_t u = 0; u < units.size(); ++u) {
    const auto& unit = units[u];
    auto body_idx = BodyIndex(unit.body, design);
    if (unit.instances.empty()) {
      throw common::InternalError(
          "BuildSpecCodegenViews",
          std::format(
              "specialization unit for body {} has no instances", body_idx));
    }

    const auto& body = *unit.body;
    const auto ordinal_map = BuildBodyProcessOrdinalMap(body);
    if (ordinal_map.nonfinal_processes.empty()) continue;

    std::vector<mir::ProcessId> unit_nonfinal_processes;
    unit_nonfinal_processes.reserve(unit.processes.size());
    for (mir::ProcessId proc_id : unit.processes) {
      if (body.arena[proc_id].kind == mir::ProcessKind::kFinal) continue;
      unit_nonfinal_processes.push_back(proc_id);
    }
    if (unit_nonfinal_processes != ordinal_map.nonfinal_processes) {
      throw common::InternalError(
          "BuildSpecCodegenViews",
          std::format(
              "body {} unit.processes non-final order does not match "
              "canonical body ordinal map",
              body_idx));
    }

    ModuleIndex rep_module_index = unit.instances[0].module_index;
    auto sched_it = modidx_to_sched_indices.find(rep_module_index.value);
    if (sched_it == modidx_to_sched_indices.end()) {
      throw common::InternalError(
          "BuildSpecCodegenViews",
          std::format(
              "body {} has {} non-final processes but representative "
              "module_index {} has no scheduled processes",
              body_idx, ordinal_map.nonfinal_processes.size(),
              rep_module_index.value));
    }
    const auto& sched_indices = sched_it->second;
    if (sched_indices.size() != ordinal_map.nonfinal_processes.size()) {
      throw common::InternalError(
          "BuildSpecCodegenViews",
          std::format(
              "body {} sched index count {} != non-final process "
              "count {}",
              body_idx, sched_indices.size(),
              ordinal_map.nonfinal_processes.size()));
    }

    views[u].processes.reserve(ordinal_map.nonfinal_processes.size());
    ForEachNonFinalProcess(
        body, ordinal_map,
        [&](uint32_t nonfinal_proc_ordinal, mir::ProcessId proc_id,
            const mir::Process& /*proc*/) {
          views[u].processes.push_back(
              SpecProcessView{
                  .nonfinal_proc_ordinal = nonfinal_proc_ordinal,
                  .layout_process_index = sched_indices[nonfinal_proc_ordinal],
                  .process_id = proc_id,
                  .func_name = std::format(
                      "body_{}_proc_{}", body_idx, nonfinal_proc_ordinal),
              });
        });
  }

  return views;
}

auto BuildCompiledModuleSpecInputs(
    const std::vector<SpecCompilationUnit>& units,
    std::vector<SpecCodegenView> views, const mir::Design& design,
    const lowering::BodyOriginProvenance* origin_provenance)
    -> std::vector<CompiledModuleSpecInput> {
  struct SiteBaseIndices {
    uint32_t deferred = 0;
    uint32_t cover = 0;
  };
  std::unordered_map<const mir::ModuleBody*, SiteBaseIndices> body_site_bases;
  {
    uint32_t deferred_base = 0;
    uint32_t cover_base = 0;
    for (const auto& body : design.module_bodies) {
      body_site_bases[&body] = {.deferred = deferred_base, .cover = cover_base};
      deferred_base +=
          static_cast<uint32_t>(body.deferred_assertion_sites.size());
      cover_base += static_cast<uint32_t>(body.immediate_cover_sites.size());
    }
  }

  std::vector<CompiledModuleSpecInput> inputs;
  inputs.reserve(units.size());
  for (size_t i = 0; i < units.size(); ++i) {
    auto body_idx = BodyIndex(units[i].body, design);
    const lowering::BodyOriginProvenance::Entry* origin_entry =
        (origin_provenance != nullptr) ? origin_provenance->Find(units[i].body)
                                       : nullptr;
    auto base_it = body_site_bases.find(units[i].body);
    SiteBaseIndices bases = (base_it != body_site_bases.end())
                                ? base_it->second
                                : SiteBaseIndices{};
    inputs.push_back(
        CompiledModuleSpecInput{
            .body = units[i].body,
            .functions = units[i].functions,
            .view = std::move(views[i]),
            .name_prefix = std::format("body_{}", body_idx),
            .origin_entry = origin_entry,
            .deferred_sites = units[i].body->deferred_assertion_sites,
            .deferred_site_base_index = bases.deferred,
            .cover_site_base_index = bases.cover,
            .module_export_targets = {},
        });
  }
  return inputs;
}

auto BuildSpecSlotInfos(
    const std::vector<SpecCompilationUnit>& units,
    std::span<const Layout::BodyRealizationInfo> body_realization_infos)
    -> std::vector<SpecSlotInfo> {
  std::unordered_map<const mir::ModuleBody*, uint32_t> body_info_index_by_ptr;
  for (uint32_t i = 0; i < body_realization_infos.size(); ++i) {
    body_info_index_by_ptr[body_realization_infos[i].body] = i;
  }

  std::vector<SpecSlotInfo> result;
  result.reserve(units.size());
  for (const auto& unit : units) {
    SpecSlotInfo info;
    uint32_t body_info_idx = 0;
    {
      auto it = body_info_index_by_ptr.find(unit.body);
      if (it == body_info_index_by_ptr.end()) {
        throw common::InternalError(
            "BuildSpecSlotInfos", "no BodyRealizationInfo for body");
      }
      body_info_idx = it->second;
    }

    const auto& body_info = body_realization_infos[body_info_idx];
    const auto& body_layout = body_info.body_layout;
    const auto& body = *unit.body;
    auto slot_count = body_info.slot_count;

    info.inline_offsets.reserve(slot_count);
    info.appendix_offsets.reserve(slot_count);
    info.shapes.reserve(slot_count);
    info.access_kinds.reserve(slot_count);
    for (uint32_t s = 0; s < slot_count; ++s) {
      info.inline_offsets.push_back(body_layout.inline_offsets[s]);
      info.appendix_offsets.push_back(body_layout.appendix_offsets[s]);
      auto shape = body.slots[s].storage_shape;
      info.shapes.push_back(shape);
      bool is_container = shape == mir::StorageShape::kOwnedContainer;
      info.access_kinds.push_back(
          is_container ? SpecSlotAccessKind::kOwnedContainer
                       : SpecSlotAccessKind::kOwnedInline);
    }
    result.push_back(std::move(info));
  }
  return result;
}

// Build per-instance local-slot connection trigger facts.
auto BuildInstanceConnectionTriggers(
    std::span<const LayoutModulePlan> module_plans,
    const std::vector<bool>& slot_has_connection_trigger)
    -> std::vector<std::vector<bool>> {
  std::vector<std::vector<bool>> instance_triggers(module_plans.size());
  for (size_t mi = 0; mi < module_plans.size(); ++mi) {
    const auto& plan = module_plans[mi];
    instance_triggers[mi].assign(plan.slot_count, false);
    for (uint32_t s = 0; s < plan.slot_count; ++s) {
      uint32_t global_slot = plan.design_state_base_slot + s;
      if (global_slot >= slot_has_connection_trigger.size()) {
        throw common::InternalError(
            "BuildInstanceConnectionTriggers",
            std::format(
                "design-global slot {} out of range for connection trigger "
                "bitmap (size={}, instance={}, local_slot={})",
                global_slot, slot_has_connection_trigger.size(), mi, s));
      }
      if (slot_has_connection_trigger[global_slot]) {
        instance_triggers[mi][s] = true;
      }
    }
  }
  return instance_triggers;
}

// Build connection-notification masks: conservative union across all
// instances of each body.
auto BuildConnectionNotificationMasks(
    const std::vector<SpecCompilationUnit>& units,
    const std::vector<SpecSlotInfo>& slot_infos,
    const std::vector<std::vector<bool>>& instance_triggers)
    -> std::vector<ConnectionNotificationMask> {
  std::vector<ConnectionNotificationMask> masks(units.size());
  for (size_t u = 0; u < units.size(); ++u) {
    auto slot_count = slot_infos[u].SlotCount();
    auto& mask = masks[u];
    mask.required.assign(slot_count, false);
    for (const auto& inst : units[u].instances) {
      auto mi = inst.module_index.value;
      const auto& inst_trig = instance_triggers.at(mi);
      if (inst_trig.size() != slot_count) {
        throw common::InternalError(
            "BuildConnectionNotificationMasks",
            std::format(
                "instance {} connection_triggers size {} != "
                "spec slot_count {}",
                mi, inst_trig.size(), slot_count));
      }
      for (uint32_t s = 0; s < slot_count; ++s) {
        if (inst_trig[s]) {
          mask.required[s] = true;
        }
      }
    }
  }
  return masks;
}

}  // namespace

auto BuildSpecPlan(
    const mir::Design& design, const Layout& layout,
    std::span<const LayoutModulePlan> module_plans,
    const lowering::BodyOriginProvenance* origin_provenance,
    std::span<const mir::DpiExportWrapperDesc> dpi_export_wrappers)
    -> SpecPlan {
  auto units = BuildSpecCompilationUnits(design);
  auto modidx_to_sched_indices = BuildModuleSchedIndices(
      layout.num_init_processes, layout.scheduled_processes);
  auto views = BuildSpecCodegenViews(units, design, modidx_to_sched_indices);
  auto slot_infos = BuildSpecSlotInfos(units, layout.body_realization_infos);

  auto instance_triggers = BuildInstanceConnectionTriggers(
      module_plans, layout.slot_has_connection_trigger);
  auto masks =
      BuildConnectionNotificationMasks(units, slot_infos, instance_triggers);

  auto inputs = BuildCompiledModuleSpecInputs(
      units, std::move(views), design, origin_provenance);

  // Precompute per-body module export targets from the design-global wrapper
  // list. Each body gets only the module-scoped wrappers that target it.
  std::vector<std::vector<ModuleExportTarget>> export_targets(inputs.size());
  for (uint32_t wi = 0; wi < dpi_export_wrappers.size(); ++wi) {
    const auto& desc = dpi_export_wrappers[wi];
    if (desc.target.scope_kind != mir::DpiExportScopeKind::kModule) continue;
    for (size_t u = 0; u < inputs.size(); ++u) {
      if (inputs[u].body != desc.target.module_target.body) continue;
      export_targets[u].push_back(
          ModuleExportTarget{
              .wrapper_index = wi,
              .function_id = desc.target.module_target.function_id,
          });
      break;
    }
  }

  // Build per-body CU-local layout contracts from the design-global Layout.
  // Map body pointers to body_realization_infos indices for contract building.
  std::unordered_map<const mir::ModuleBody*, uint32_t> body_info_index_by_ptr;
  for (uint32_t i = 0; i < layout.body_realization_infos.size(); ++i) {
    body_info_index_by_ptr[layout.body_realization_infos[i].body] = i;
  }

  std::vector<SpecLayoutContract> layout_contracts;
  layout_contracts.reserve(inputs.size());
  for (size_t i = 0; i < inputs.size(); ++i) {
    auto it = body_info_index_by_ptr.find(units[i].body);
    if (it == body_info_index_by_ptr.end()) {
      throw common::InternalError(
          "BuildSpecPlan", "no BodyRealizationInfo for body");
    }
    auto body_info_idx = it->second;
    const auto& bri = layout.body_realization_infos[body_info_idx];

    SpecLayoutContract contract;
    contract.process_layouts.reserve(inputs[i].view.processes.size());
    for (const auto& pv : inputs[i].view.processes) {
      contract.process_layouts.push_back(
          &layout.processes.at(pv.layout_process_index));
    }
    contract.slot_specs = bri.slot_specs;
    contract.slot_has_behavioral_trigger = bri.slot_has_behavioral_trigger;
    contract.representative_slot_base =
        layout.body_representative_base_slots[body_info_idx];

    // Pre-compute cross-body behavioral trigger bitmap for this body.
    // A slot has a cross-body behavioral trigger if its design-global
    // representative is marked in the design-global bitmap but NOT in
    // the body-local bitmap.
    auto slot_count = bri.slot_count;
    contract.slot_has_cross_body_behavioral_trigger.resize(slot_count, false);
    for (uint32_t s = 0; s < slot_count; ++s) {
      auto global_slot = contract.representative_slot_base + s;
      if (global_slot < layout.slot_has_design_behavioral_trigger.size() &&
          layout.slot_has_design_behavioral_trigger[global_slot] &&
          !bri.slot_has_behavioral_trigger[s]) {
        contract.slot_has_cross_body_behavioral_trigger[s] = true;
      }
    }

    layout_contracts.push_back(std::move(contract));
  }

  SpecPlan plan{
      .units = std::move(units),
      .slot_infos = std::move(slot_infos),
      .inputs = std::move(inputs),
      .connection_notification_masks = std::move(masks),
      .module_export_targets = std::move(export_targets),
      .layout_contracts = std::move(layout_contracts),
  };

  // Wire up per-input pointers/spans. Safe through moves because vector move
  // transfers the heap buffer without relocating elements.
  for (size_t i = 0; i < plan.inputs.size(); ++i) {
    plan.inputs[i].spec_slot_info = &plan.slot_infos[i];
    plan.inputs[i].connection_notification_mask =
        &plan.connection_notification_masks[i];
    plan.inputs[i].module_export_targets = plan.module_export_targets[i];
    plan.inputs[i].layout_contract = &plan.layout_contracts[i];
  }

  return plan;
}

}  // namespace lyra::lowering::mir_to_llvm
