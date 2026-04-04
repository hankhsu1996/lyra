#include "lyra/llvm_backend/lower.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <expected>
#include <format>
#include <iterator>
#include <memory>
#include <mutex>
#include <span>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <variant>
#include <vector>

#include <llvm/ADT/StringMap.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/TargetParser/Host.h>
#include <llvm/TargetParser/SubtargetFeature.h>

#include "lyra/common/internal_error.hpp"
#include "lyra/llvm_backend/codegen_session.hpp"
#include "lyra/llvm_backend/connection_analysis.hpp"
#include "lyra/llvm_backend/connection_kernel_collection.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/dpi_abi.hpp"
#include "lyra/llvm_backend/init_descriptor_utils.hpp"
#include "lyra/llvm_backend/layout/layout.hpp"
#include "lyra/llvm_backend/observable_descriptor_utils.hpp"
#include "lyra/llvm_backend/process.hpp"
#include "lyra/llvm_backend/process_meta_utils.hpp"
#include "lyra/llvm_backend/runtime_abi_codegen.hpp"
#include "lyra/llvm_backend/storage_construction_recipe_builder.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/module.hpp"
#include "lyra/mir/placement.hpp"
#include "lyra/mir/routine.hpp"
#include "lyra/mir/statement.hpp"
#include "lyra/mir/terminator.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

struct ObservableDescriptorOwnerRefFields {
  uint32_t storage_owner_ref = 0;
  uint32_t flags = 0;
};

auto BuildBodyObservableDescriptorOwnerRefFields(
    ObservableOwnerSlotId owner, uint32_t base_slot, uint32_t slot_count)
    -> ObservableDescriptorOwnerRefFields {
  const uint32_t raw_owner = owner.Raw();
  const bool owner_is_in_body =
      raw_owner >= base_slot && raw_owner < base_slot + slot_count;
  if (owner_is_in_body) {
    return {.storage_owner_ref = raw_owner - base_slot, .flags = 0};
  }
  return {
      .storage_owner_ref = raw_owner,
      .flags = runtime::kObservableFlagOwnerAbsolute};
}

auto BuildPackageObservableDescriptorOwnerRefFields(ObservableOwnerSlotId owner)
    -> ObservableDescriptorOwnerRefFields {
  return {
      .storage_owner_ref = owner.Raw(),
      .flags = runtime::kObservableFlagOwnerAbsolute |
               runtime::kObservableFlagPackageGlobal};
}

struct ObservableDescriptorShapeFields {
  uint32_t total_bytes = 0;
  uint32_t storage_kind = 0;
  uint32_t value_lane_offset = 0;
  uint32_t value_lane_bytes = 0;
  uint32_t unk_lane_offset = 0;
  uint32_t unk_lane_bytes = 0;
  uint32_t bit_width = 0;
  uint32_t trace_kind = 0;
};

auto BuildObservableDescriptorShapeFields(const CanonicalObservableShape& shape)
    -> ObservableDescriptorShapeFields {
  ObservableDescriptorShapeFields out{
      .total_bytes = shape.storage.total_bytes,
      .storage_kind = static_cast<uint32_t>(shape.storage.storage_kind),
      .bit_width = shape.trace.bit_width,
      .trace_kind = static_cast<uint32_t>(shape.trace.trace_kind),
  };
  if (const auto& lanes = shape.storage.packed4_lanes; lanes.has_value()) {
    out.value_lane_offset = lanes->value_lane_byte_offset;
    out.value_lane_bytes = lanes->value_lane_byte_size;
    out.unk_lane_offset = lanes->unk_lane_byte_offset;
    out.unk_lane_bytes = lanes->unk_lane_byte_size;
  }
  return out;
}

auto MakeObservableDescriptorEntry(
    uint32_t storage_byte_offset, uint32_t local_name_pool_off,
    const ObservableDescriptorOwnerRefFields& refs,
    const ObservableDescriptorShapeFields& sf, uint32_t storage_domain,
    uint32_t local_signal_id = UINT32_MAX)
    -> runtime::ObservableDescriptorEntry {
  return runtime::ObservableDescriptorEntry{
      .storage_byte_offset = storage_byte_offset,
      .total_bytes = sf.total_bytes,
      .storage_kind = sf.storage_kind,
      .value_lane_offset = sf.value_lane_offset,
      .value_lane_bytes = sf.value_lane_bytes,
      .unk_lane_offset = sf.unk_lane_offset,
      .unk_lane_bytes = sf.unk_lane_bytes,
      .bit_width = sf.bit_width,
      .local_name_pool_off = local_name_pool_off,
      .trace_kind = sf.trace_kind,
      .storage_owner_ref = refs.storage_owner_ref,
      .flags = refs.flags,
      .storage_domain = storage_domain,
      .local_signal_id = local_signal_id,
  };
}

// Compile-time param slot template entry. Keyed by body_local_slot
// to establish explicit ordering contract with per-instance payloads.
struct ParamSlotTemplateEntry {
  uint32_t body_local_slot;
  runtime::ParamInitSlotEntry slot;
};

// Sorted param slot template for one body. Entries are sorted by
// body_local_slot at construction time so per-instance payload emission
// is a pure linear merge with no per-instance normalization.
struct ParamSlotTemplate {
  std::vector<ParamSlotTemplateEntry> entries;
};

using BodyParamTemplateMap =
    std::unordered_map<mir::ModuleBodyId, ParamSlotTemplate>;

// Sorted reference to one const-block init entry. Built once per instance,
// used for a single merge walk against the sorted template.
struct SortedParamInitRef {
  uint32_t body_local_slot;
  const mir::ConstSlotInit* init;
};

// Sort and validate one instance's const-block param inits. Returns a
// sorted, duplicate-free reference list for merge-walk against the body
// template. Throws on duplicate body_local_slot entries.
auto ValidateAndSortParamInits(
    const mir::InstanceConstBlock& const_block, size_t inst_idx)
    -> std::vector<SortedParamInitRef> {
  std::vector<SortedParamInitRef> refs;
  refs.reserve(const_block.slot_inits.size());
  for (const auto& si : const_block.slot_inits) {
    refs.push_back(
        SortedParamInitRef{
            .body_local_slot = si.body_local_slot,
            .init = &si,
        });
  }
  std::ranges::sort(refs, {}, &SortedParamInitRef::body_local_slot);
  for (size_t k = 1; k < refs.size(); ++k) {
    if (refs[k].body_local_slot == refs[k - 1].body_local_slot) {
      throw common::InternalError(
          "BuildParamPayloads",
          std::format(
              "instance {} const block has duplicate param init "
              "for body_local_slot {}",
              inst_idx, refs[k].body_local_slot));
    }
  }
  return refs;
}

// Check that no sorted init entry precedes the current template slot.
// Throws if a stray init slot exists below body_local_slot_limit.
void CheckNoStrayInitBefore(
    size_t inst_idx, std::span<const SortedParamInitRef> sorted_inits,
    size_t init_idx, uint32_t body_local_slot_limit) {
  if (init_idx < sorted_inits.size() &&
      sorted_inits[init_idx].body_local_slot < body_local_slot_limit) {
    throw common::InternalError(
        "BuildParamPayloads",
        std::format(
            "instance {} const block has param init for "
            "body_local_slot {} but body template does not "
            "contain that slot",
            inst_idx, sorted_inits[init_idx].body_local_slot));
  }
}

// Pre-lower each instance's const_block to canonical storage bytes
// in body-template order (sorted by body_local_slot).
// Takes narrow scalar inputs: only the per-instance data it reads.
auto BuildParamPayloads(
    std::span<const uint32_t> instance_body_group,
    std::span<const uint32_t> design_base_slots,
    std::span<const mir::InstanceConstBlock> const_blocks, const Layout& layout,
    const BodyParamTemplateMap& body_param_templates)
    -> std::vector<std::vector<uint8_t>> {
  if (instance_body_group.size() != const_blocks.size()) {
    throw common::InternalError(
        "BuildParamPayloads",
        std::format(
            "instance_body_group size {} != const_blocks size {}",
            instance_body_group.size(), const_blocks.size()));
  }
  if (design_base_slots.size() != const_blocks.size()) {
    throw common::InternalError(
        "BuildParamPayloads",
        std::format(
            "design_base_slots size {} != const_blocks size {}",
            design_base_slots.size(), const_blocks.size()));
  }

  std::vector<std::vector<uint8_t>> payloads;
  payloads.resize(const_blocks.size());
  for (size_t inst_idx = 0; inst_idx < const_blocks.size(); ++inst_idx) {
    const auto& const_block = const_blocks[inst_idx];
    auto& payload = payloads[inst_idx];

    if (const_block.slot_inits.empty()) continue;
    uint32_t bg = instance_body_group[inst_idx];
    if (bg >= layout.body_realization_infos.size()) {
      throw common::InternalError(
          "BuildParamPayloads",
          std::format(
              "instance {} body_group {} >= body_realization_infos size {}",
              inst_idx, bg, layout.body_realization_infos.size()));
    }
    auto body_id = layout.body_realization_infos[bg].body_id;
    auto tmpl_it = body_param_templates.find(body_id);
    if (tmpl_it == body_param_templates.end()) {
      throw common::InternalError(
          "BuildParamPayloads",
          std::format(
              "instance {} has param inits but no body param template",
              inst_idx));
    }
    const auto& tmpl = tmpl_it->second.entries;

    auto sorted_inits = ValidateAndSortParamInits(const_block, inst_idx);
    uint32_t design_base_slot = design_base_slots[inst_idx];

    // Merge-walk: emit payload bytes in template order, validating
    // that every init slot appears in the template.
    size_t init_idx = 0;
    for (const auto& te : tmpl) {
      CheckNoStrayInitBefore(
          inst_idx, sorted_inits, init_idx, te.body_local_slot);
      if (init_idx < sorted_inits.size() &&
          sorted_inits[init_idx].body_local_slot == te.body_local_slot) {
        const auto& init = *sorted_inits[init_idx].init;
        uint32_t abs_slot = design_base_slot + init.body_local_slot;
        const auto& spec = layout.design.slot_storage_specs[abs_slot];
        LowerIntegralConstantToCanonicalBytes(init.value, spec, payload);
        ++init_idx;
      } else {
        payload.resize(payload.size() + te.slot.byte_size, 0);
      }
    }
    if (init_idx != sorted_inits.size()) {
      throw common::InternalError(
          "BuildParamPayloads",
          std::format(
              "instance {} const block has param init for "
              "body_local_slot {} but body template does not "
              "contain that slot",
              inst_idx, sorted_inits[init_idx].body_local_slot));
    }
  }
  return payloads;
}

// Build the pure-data construction program from parallel topology arrays.
// Pools path strings and param payloads; entries are in strict ModuleIndex
// order. The runtime relies on this order for instance_id and flat
// slot-base allocation.
auto BuildConstructionProgram(
    std::span<const uint32_t> instance_body_group, const Layout& layout,
    const mir::InstanceTable& instance_table,
    std::span<const std::vector<uint8_t>> param_payloads)
    -> ConstructionProgramData {
  auto instance_count = static_cast<uint32_t>(instance_body_group.size());
  ConstructionProgramData prog;
  prog.entries.reserve(instance_count);

  for (uint32_t mi = 0; mi < instance_count; ++mi) {
    runtime::ConstructionProgramEntry entry{};

    entry.body_group = instance_body_group[mi];

    const auto& path = instance_table.entries[mi].full_path;
    entry.path_offset = static_cast<uint32_t>(prog.path_pool.size());
    prog.path_pool.insert(prog.path_pool.end(), path.begin(), path.end());
    prog.path_pool.push_back(0);

    const auto& payload = param_payloads[mi];
    if (!payload.empty()) {
      entry.param_offset = static_cast<uint32_t>(prog.param_pool.size());
      entry.param_size = static_cast<uint32_t>(payload.size());
      prog.param_pool.insert(
          prog.param_pool.end(), payload.begin(), payload.end());
    }

    const auto& sizes = layout.instance_storage_sizes[mi];
    entry.realized_inline_size = sizes.inline_bytes;
    entry.realized_appendix_size = sizes.appendix_bytes;

    prog.entries.push_back(entry);
  }

  return prog;
}

// Truly file-local LLVM initialization helpers.

void InitializeLlvmTargets() {
  static std::once_flag flag;
  std::call_once(flag, [] {
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();
  });
}

void SetHostDataLayout(llvm::Module& module) {
  InitializeLlvmTargets();

  std::string triple = llvm::sys::getDefaultTargetTriple();
  std::string cpu = llvm::sys::getHostCPUName().str();

  llvm::StringMap<bool> feature_map;
  llvm::sys::getHostCPUFeatures(feature_map);
  llvm::SubtargetFeatures subtarget_features;
  for (const auto& kv : feature_map) {
    if (kv.getValue()) {
      subtarget_features.AddFeature(kv.getKey().str());
    }
  }
  std::string features = subtarget_features.getString();

  std::string error;
  const llvm::Target* target =
      llvm::TargetRegistry::lookupTarget(triple, error);
  if (target == nullptr) {
    throw common::InternalError(
        "SetHostDataLayout",
        std::format("failed to lookup target for '{}': {}", triple, error));
  }

  std::unique_ptr<llvm::TargetMachine> tm(target->createTargetMachine(
      triple, cpu, features, llvm::TargetOptions(), std::nullopt));
  if (tm == nullptr) {
    throw common::InternalError(
        "SetHostDataLayout",
        std::format("failed to create TargetMachine for '{}'", triple));
  }

  module.setTargetTriple(triple);
  module.setDataLayout(tm->createDataLayout());
}

void RegisterMonitorInfo(
    Context& context, const mir::Arena& arena, mir::ProcessId proc_id) {
  const auto& process = arena[proc_id];
  for (const auto& block : process.blocks) {
    for (const auto& instr : block.statements) {
      const auto* effect = std::get_if<mir::Effect>(&instr.data);
      if (effect == nullptr) continue;
      const auto* monitor = std::get_if<mir::MonitorEffect>(&effect->op);
      if (monitor == nullptr) continue;

      Context::MonitorLayout mon_layout{
          .offsets = monitor->offsets,
          .byte_sizes = monitor->byte_sizes,
          .total_size = monitor->prev_buffer_size,
      };
      context.RegisterMonitorLayout(
          monitor->check_program, std::move(mon_layout));

      Context::MonitorSetupInfo setup_info{
          .check_program = monitor->check_program,
      };
      context.RegisterMonitorSetupInfo(
          monitor->setup_program, std::move(setup_info));
    }
  }
}

// Named transfer type for body-relative behavioral dirty-trigger bitmaps.
// Each bitmap is indexed by body-local slot id [0, slot_count).
// Keyed by raw body_id.value (not strong-ID) for lookup during
// BodyRealizationInfo population.
struct BodyBehavioralTriggerBitmaps {
  std::unordered_map<uint32_t, std::vector<bool>> by_body_id_value;
};

// Build the body-relative behavioral dirty-trigger contract from MIR.
//
// A body-local slot is marked true iff any behavioral wait dependency in
// the body's artifact repertoire references it. This covers:
//   - process waits (always_ff, always_comb, initial @(...) triggers)
//   - comb triggers (always_comb processes have Wait terminators)
//   - late-bound dependency slots (index variables for dynamic edge triggers
//     that rebind watchers subscribe on at runtime)
//
// These are the same processes stored in ModuleBody::processes and
// referenced by LayoutModulePlan::body_processes. The only exclusion is
// kFinal processes, which run at simulation end and never produce Wait
// terminators. All behavioral wait producers in MIR are represented as
// non-final processes with Wait terminators; there is no other behavioral
// trigger source in the current architecture.
//
// Only kModuleLocal triggers contribute to this bitmap. kDesignGlobal
// triggers in body processes are handled by the separate design-global
// behavioral contract (BuildDesignGlobalBehavioralTriggerBitmap).
auto BuildBodyBehavioralDirtyTriggerBitmaps(
    std::span<const LayoutModulePlan> module_plans, const mir::Design& design)
    -> BodyBehavioralTriggerBitmaps {
  BodyBehavioralTriggerBitmaps result;

  for (const auto& plan : module_plans) {
    auto body_id_val = plan.body_id.value;
    if (result.by_body_id_value.contains(body_id_val)) continue;

    std::vector<bool> bitmap(plan.slot_count, false);
    const auto& body = design.module_bodies.at(body_id_val);

    for (const auto& proc_id : plan.body_processes) {
      const auto& process = body.arena[proc_id];
      if (process.kind == mir::ProcessKind::kFinal) continue;

      // Helper: mark a module-local slot in the bitmap with bounds check.
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
          // Mark the trigger signal itself.
          if (trigger.signal.scope == mir::SignalRef::Scope::kModuleLocal) {
            mark_local(
                static_cast<uint32_t>(trigger.signal.id),
                "module-local trigger");
          }
          // Mark late-bound dependency slots. These are the index
          // variables that rebind watchers subscribe on at runtime.
          // They are part of the compiled behavioral dependency
          // repertoire of the wait site.
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

// Transfer body-relative behavioral trigger bitmaps into their
// architectural owner (BodyRealizationInfo on Layout).
// Build the design-global behavioral dirty-trigger contract from MIR.
//
// Currently covers two explicit sources of design-global behavioral waits:
//   - init processes (design-level, data in design_arena)
//   - body processes that wait on kDesignGlobal signals
//
// Init processes are design-scoped and must use only kDesignGlobal signals.
// Any kModuleLocal trigger in an init process is an invariant violation.
//
// Body processes may have mixed-scope waits. Only their kDesignGlobal
// triggers contribute here; kModuleLocal triggers are owned by the
// body-relative behavioral contract.
//
// This is NOT a fallback for body-local triggers.
// Result is keyed by canonical storage-owner slot identity.
//
// Completeness note: this helper currently covers init processes and
// body processes with kDesignGlobal waits. Connection processes
// (kernelized and non-kernelized) are connection-owned, not
// behavioral-owned; their triggers belong in slot_has_connection_trigger.
// If other design-global behavioral process owners are added, they
// must be included here explicitly.
auto BuildDesignGlobalBehavioralTriggerBitmap(
    std::span<const LayoutModulePlan> module_plans, const mir::Design& design,
    const mir::Arena& design_arena, const DesignLayout& design_layout)
    -> std::vector<bool> {
  auto num_slots = design_layout.slots.size();
  std::vector<bool> bitmap(num_slots, false);

  // Helper: mark a single design-global trigger slot in the bitmap.
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
  // Init processes must use kDesignGlobal signals exclusively.
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

  // Body processes: data lives in per-body arenas.
  // Only kDesignGlobal triggers contribute; kModuleLocal triggers are
  // owned by the body-relative behavioral contract.
  for (const auto& plan : module_plans) {
    const auto& body = design.module_bodies.at(plan.body_id.value);
    for (const auto& proc_id : plan.body_processes) {
      const auto& process = body.arena[proc_id];
      if (process.kind == mir::ProcessKind::kFinal) continue;
      for (const auto& block : process.blocks) {
        const auto* wait = std::get_if<mir::Wait>(&block.terminator.data);
        if (wait == nullptr) continue;
        for (const auto& trigger : wait->triggers) {
          if (trigger.signal.scope != mir::SignalRef::Scope::kDesignGlobal) {
            continue;
          }
          mark_global_slot(
              common::SlotId{static_cast<uint32_t>(trigger.signal.id)});
        }
      }
    }
  }

  return bitmap;
}

void PopulateBodyBehavioralTriggerContracts(
    std::span<const LayoutModulePlan> module_plans, const mir::Design& design,
    Layout& layout) {
  auto bitmaps = BuildBodyBehavioralDirtyTriggerBitmaps(module_plans, design);
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
      info.slot_has_behavioral_trigger = std::move(it->second);
    } else {
      info.slot_has_behavioral_trigger.assign(info.slot_count, false);
    }
  }
}

auto BuildSpecCompilationUnits(const mir::Design& design)
    -> std::vector<SpecCompilationUnit> {
  std::vector<SpecCompilationUnit> units;
  std::unordered_map<uint32_t, size_t> body_to_unit;

  uint32_t module_idx = 0;
  for (const auto& element : design.elements) {
    const auto* mod = std::get_if<mir::Module>(&element);
    if (mod == nullptr) continue;

    auto body_id = mod->body_id;
    SpecInstanceBinding binding{
        .module_index = ModuleIndex{module_idx},
    };

    auto [it, inserted] = body_to_unit.try_emplace(body_id.value, units.size());
    if (inserted) {
      const auto& body = design.module_bodies.at(body_id.value);
      units.push_back(
          SpecCompilationUnit{
              .body_id = body_id,
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

// Build module_index -> ordered list of scheduled process indices.
// Each entry contains the layout-order indices of that instance's non-final
// processes in scheduled_processes.
auto BuildModuleSchedIndices(const Layout& layout)
    -> std::unordered_map<uint32_t, std::vector<uint32_t>> {
  std::unordered_map<uint32_t, std::vector<uint32_t>> result;
  for (size_t i = layout.num_init_processes;
       i < layout.scheduled_processes.size(); ++i) {
    const auto& sp = layout.scheduled_processes[i];
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
    if (unit.instances.empty()) {
      throw common::InternalError(
          "BuildSpecCodegenViews",
          std::format(
              "specialization unit for body {} has no instances",
              unit.body_id.value));
    }

    const auto& body = design.module_bodies.at(unit.body_id.value);
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
              unit.body_id.value));
    }

    // Use first instance as representative for scheduled-process indexing
    ModuleIndex rep_module_index = unit.instances[0].module_index;
    auto sched_it = modidx_to_sched_indices.find(rep_module_index.value);
    if (sched_it == modidx_to_sched_indices.end()) {
      throw common::InternalError(
          "BuildSpecCodegenViews",
          std::format(
              "body {} has {} non-final processes but representative "
              "module_index {} has no scheduled processes",
              unit.body_id.value, ordinal_map.nonfinal_processes.size(),
              rep_module_index.value));
    }
    const auto& sched_indices = sched_it->second;
    if (sched_indices.size() != ordinal_map.nonfinal_processes.size()) {
      throw common::InternalError(
          "BuildSpecCodegenViews",
          std::format(
              "body {} sched index count {} != non-final process "
              "count {}",
              unit.body_id.value, sched_indices.size(),
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
                      "body_{}_proc_{}", unit.body_id.value,
                      nonfinal_proc_ordinal),
              });
        });
  }

  return views;
}

auto BuildCompiledModuleSpecInputs(
    const std::vector<SpecCompilationUnit>& units,
    std::vector<SpecCodegenView> views)
    -> std::vector<CompiledModuleSpecInput> {
  std::vector<CompiledModuleSpecInput> inputs;
  inputs.reserve(units.size());
  for (size_t i = 0; i < units.size(); ++i) {
    inputs.push_back(
        CompiledModuleSpecInput{
            .body_id = units[i].body_id,
            .processes = units[i].processes,
            .functions = units[i].functions,
            .view = std::move(views[i]),
        });
  }
  return inputs;
}

auto BuildSpecSlotInfos(
    const std::vector<SpecCompilationUnit>& units, const Layout& layout,
    const mir::Design& design) -> std::vector<SpecSlotInfo> {
  // Build body-id-to-index table once for O(1) lookup per unit.
  std::unordered_map<uint32_t, uint32_t> body_info_index_by_body_id_value;
  for (uint32_t i = 0; i < layout.body_realization_infos.size(); ++i) {
    body_info_index_by_body_id_value[layout.body_realization_infos[i]
                                         .body_id.value] = i;
  }

  std::vector<SpecSlotInfo> result;
  result.reserve(units.size());
  for (const auto& unit : units) {
    SpecSlotInfo info;
    info.body_id = unit.body_id;
    uint32_t body_info_idx = 0;
    {
      auto it = body_info_index_by_body_id_value.find(unit.body_id.value);
      if (it == body_info_index_by_body_id_value.end()) {
        throw common::InternalError(
            "BuildSpecSlotInfos",
            std::format(
                "no BodyRealizationInfo for body {}", unit.body_id.value));
      }
      body_info_idx = it->second;
      info.body_realization_info_index = body_info_idx;
    }

    // Build SpecSlotInfo from body-local layout data only.
    // No design-global slot table, no representative instance, no
    // flat_slot_base. Specialization grouping guarantees all instances
    // of the same body have identical slot specs.
    const auto& body_info = layout.body_realization_infos[body_info_idx];
    const auto& body_layout = body_info.body_layout;
    const auto& body = design.module_bodies.at(unit.body_id.value);
    auto slot_count = body_info.slot_count;

    info.inline_offsets.reserve(slot_count);
    info.shapes.reserve(slot_count);
    info.access_kinds.reserve(slot_count);
    for (uint32_t s = 0; s < slot_count; ++s) {
      info.inline_offsets.push_back(body_layout.inline_offsets[s]);
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

// Per-slot observation accumulator for comb template extraction.
struct CombSlotAccum {
  uint32_t byte_offset = 0;
  uint32_t byte_size = 0;
  bool is_full_slot = false;
  bool is_design_global = false;
  // Final design-global slot id for canonical sort ordering.
  uint32_t final_global_id = 0;
};

// Comb template entry comparator matching the canonical observable order.
// Sort by final design-global slot_id (absolute after per-instance
// relocation). For the body-shaped template, we sort by an equivalent
// final_global_id (base_slot + body_relative_id for module-local,
// design-global id directly for kDesignGlobal). This is valid because
// per-body entry order is invariant under constant base-slot translation.
// Total order: primary=final_global_id, secondary=scope, tertiary=id.
auto CompareCombSlotsByFinalObservableOrder(
    const std::pair<ScopedSignalKey, CombSlotAccum>& a,
    const std::pair<ScopedSignalKey, CombSlotAccum>& b) -> bool {
  if (a.second.final_global_id != b.second.final_global_id) {
    return a.second.final_global_id < b.second.final_global_id;
  }
  if (a.first.scope != b.first.scope) {
    return static_cast<uint8_t>(a.first.scope) <
           static_cast<uint8_t>(b.first.scope);
  }
  return a.first.id < b.first.id;
}

}  // namespace

auto CompileModuleSpecSession(
    Context& context, const mir::Design& design,
    const CompiledModuleSpecInput& input) -> Result<CompiledModuleSpec> {
  // Set the body arena and origin scope for this compilation session.
  const auto& body = design.module_bodies.at(input.body_id.value);
  Context::ArenaScope arena_scope(context, &body.arena);

  // Establish body-scoped origin resolution so OriginId lookups during
  // per-statement codegen resolve against the correct body's origin map.
  std::optional<lowering::OriginMapLookup::BodyScope> origin_scope;
  if (context.GetOriginLookup() != nullptr) {
    origin_scope.emplace(*context.GetOriginLookup(), input.body_id);
  }

  // Clear per-spec state: body-local FunctionIds are 0-based per body,
  // so registrations from a previous spec session would collide.
  context.ClearModuleScopedFunctions();

  // Step 1: Register monitor info for body processes
  for (mir::ProcessId proc_id : input.processes) {
    RegisterMonitorInfo(context, body.arena, proc_id);
  }

  // Step 2: Register module-scoped function lowering metadata.
  // All body functions share the same spec slot info. Observer ABI vs
  // regular ABI is determined by mir::IsObserverProgram(runtime_kind) at
  // declare/define time, not by metadata here.
  // spec_slot_info is set on context by CompileDesignProcesses before
  // entering this function. It must be non-null for module-scoped access.
  if (context.GetSpecSlotInfo() == nullptr) {
    throw common::InternalError(
        "CompileModuleSpecSession",
        "spec_slot_info not set on context before body compilation");
  }
  for (mir::FunctionId func_id : input.functions) {
    context.RegisterModuleScopedFunction(
        func_id, {.spec_slot_info = context.GetSpecSlotInfo(),
                  .connection_notification_mask =
                      context.GetConnectionNotificationMask()});
  }

  // Step 3: Declare all body functions
  std::vector<std::pair<mir::FunctionId, llvm::Function*>> declared_funcs;
  std::unordered_set<uint32_t> seen_func_ids;
  for (mir::FunctionId func_id : input.functions) {
    if (!seen_func_ids.insert(func_id.value).second) continue;
    auto llvm_func_or_err = DeclareMirFunction(
        context, func_id,
        std::format("body_{}_func_{}", input.body_id.value, func_id.value));
    if (!llvm_func_or_err) return std::unexpected(llvm_func_or_err.error());
    declared_funcs.emplace_back(func_id, *llvm_func_or_err);
  }

  // Step 4: Define all body functions
  for (const auto& [func_id, llvm_func] : declared_funcs) {
    auto result = DefineMirFunction(context, func_id, llvm_func);
    if (!result) return std::unexpected(result.error());
  }

  // Step 5: Codegen all body-owned processes
  CompiledModuleSpec product{
      .body_id = input.body_id,
      .process_functions = {},
      .wait_sites = {},
      .process_triggers = {},
  };

  for (const auto& proc_view : input.view.processes) {
    context.SetCurrentProcess(proc_view.layout_process_index);

    const auto& mir_process = body.arena[proc_view.process_id];
    auto func_result = GenerateSharedProcessFunction(
        context, mir_process, proc_view.func_name);
    if (!func_result) return std::unexpected(func_result.error());

    product.process_functions.push_back(func_result->function);
    product.process_triggers.push_back(std::move(func_result->process_trigger));

    product.wait_sites.insert(
        product.wait_sites.end(),
        std::make_move_iterator(func_result->wait_sites.begin()),
        std::make_move_iterator(func_result->wait_sites.end()));
  }

  return product;
}

auto ExtractRealizationData(
    const mir::ConstructionInput& construction,
    std::span<const mir::SlotDesc> slots,
    std::span<const mir::SlotTraceProvenance> slot_trace_provenance,
    std::span<const char> slot_trace_string_pool) -> RealizationData {
  RealizationData realization;

  if (construction.objects.size() != construction.const_blocks.size()) {
    throw common::InternalError(
        "ExtractRealizationData",
        std::format(
            "construction objects/const_blocks size mismatch: {} vs {}",
            construction.objects.size(), construction.const_blocks.size()));
  }

  realization.slot_types.reserve(slots.size());
  realization.slot_kinds.reserve(slots.size());
  for (const auto& slot : slots) {
    realization.slot_types.push_back(slot.type);
    realization.slot_kinds.push_back(slot.kind);
  }

  realization.slot_trace_provenance.assign(
      slot_trace_provenance.begin(), slot_trace_provenance.end());
  realization.slot_trace_string_pool.assign(
      slot_trace_string_pool.begin(), slot_trace_string_pool.end());

  return realization;
}

auto CompileDesignProcesses(const LoweringInput& input)
    -> Result<CodegenSession> {
  if (input.body_timescales == nullptr) {
    throw common::InternalError(
        "CompileDesignProcesses", "body_timescales must be non-null");
  }

  // Phase 0: Backend/session setup
  auto llvm_ctx = std::make_unique<llvm::LLVMContext>();
  auto module = std::make_unique<llvm::Module>("lyra_module", *llvm_ctx);

  SetHostDataLayout(*module);

  bool force_two_state = input.force_two_state;

  auto slot_info =
      BuildSlotInfo(input.design->slots, *input.type_arena, force_two_state);

  // Preliminary layout: used by connection collection for observation
  // resolution (needs slot specs, not byte offsets).
  auto pre_layout = BuildDesignLayout(
      slot_info, *input.type_arena, module->getDataLayout(), force_two_state, 0,
      {});

  // Extract narrow layout-planning inputs from design.
  // module_plans and module_base_slots are produced once and consumed by
  // BuildLayout and wrapper generation respectively.
  //
  // Contract: module_plans[i] corresponds to the i-th Module element in
  // design.elements (skipping non-Module variants). This ordering matches
  // placement.instances[i], which is how ModuleIndex is assigned during
  // MIR construction. BuildLayout and wrapper generation depend on this.
  std::vector<LayoutModulePlan> module_plans;
  {
    uint32_t module_idx = 0;
    for (const auto& element : input.design->elements) {
      const auto* mod = std::get_if<mir::Module>(&element);
      if (mod == nullptr) continue;
      const auto& body = input.design->module_bodies.at(mod->body_id.value);
      const auto& obj = input.construction->objects.at(module_idx);
      module_plans.push_back(
          LayoutModulePlan{
              .body_processes = body.processes,
              .body_id = mod->body_id,
              .design_state_base_slot = obj.design_state_base_slot,
              .slot_count = obj.slot_count,
          });
      ++module_idx;
    }
    if (module_idx != input.construction->objects.size()) {
      throw common::InternalError(
          "CompileDesignProcesses",
          std::format(
              "module count {} from elements does not match construction "
              "object count {}",
              module_idx, input.construction->objects.size()));
    }
  }

  // Connection-kernel collection uses pre-forwarding layout for trigger
  // observation resolution. Only slot_storage_specs, storage_spec_arena,
  // and slot_to_index are consumed.
  // No later phase may re-collect connection kernels.
  auto connection_collection = CollectConnectionKernels(
      input.design->connection_processes, *input.mir_arena,
      SlotSpecView{
          .slot_to_index = pre_layout.slot_to_index,
          .slot_storage_specs = pre_layout.slot_storage_specs,
          .storage_spec_arena = pre_layout.storage_spec_arena,
      });

  // Connection analysis: classify slot usage and relay candidates.
  // Connection edges use original slot IDs; no canonical-owner aliasing.
  auto connection_analysis = AnalyzeConnections(
      std::move(connection_collection.kernel_entries), module_plans,
      *input.design, *input.mir_arena);

  // Build instance slot ranges for body-local appendix layout.
  uint32_t num_package_slots_for_layout = 0;
  std::vector<InstanceSlotRange> instance_ranges;
  instance_ranges.reserve(module_plans.size());
  if (!module_plans.empty()) {
    num_package_slots_for_layout = module_plans[0].design_state_base_slot;
  } else {
    num_package_slots_for_layout =
        static_cast<uint32_t>(input.design->slots.size());
  }
  for (const auto& plan : module_plans) {
    instance_ranges.push_back(
        InstanceSlotRange{
            .base_slot = plan.design_state_base_slot,
            .slot_count = plan.slot_count,
            .body_id = plan.body_id,
        });
  }

  // Build final layout: every body-local slot owns storage.
  auto design_layout = BuildDesignLayout(
      slot_info, *input.type_arena, module->getDataLayout(), force_two_state,
      num_package_slots_for_layout, instance_ranges);

  // Build body-owned storage layouts from body-local MIR inputs.
  // Iterates the body inventory directly. No design-global forwarding,
  // no instance placements, no design_state_base_slot.
  std::unordered_map<uint32_t, BodyStorageLayout> body_storage_layouts;
  for (uint32_t bi = 0; bi < input.design->module_bodies.size(); ++bi) {
    const auto& body = input.design->module_bodies[bi];
    if (body.slots.empty()) continue;
    body_storage_layouts.emplace(
        bi,
        BuildBodyStorageLayout(
            body, *input.type_arena, module->getDataLayout(), force_two_state));
  }

  auto layout = std::make_unique<Layout>(BuildLayout(
      input.design->init_processes,
      std::move(connection_analysis.connection_edges),
      std::move(connection_collection.non_kernelized_processes), module_plans,
      *input.design, *input.mir_arena, *input.type_arena,
      std::move(design_layout), body_storage_layouts, input.body_timescales,
      *llvm_ctx, module->getDataLayout(), force_two_state));

  // Populate body-relative behavioral dirty-trigger contracts on
  // BodyRealizationInfo. Must complete before Phase 4 codegen reads them
  // via Context::RequiresDirtyPropagation.
  PopulateBodyBehavioralTriggerContracts(module_plans, *input.design, *layout);

  // Populate design-global behavioral dirty-trigger contract.
  // Covers init processes and body processes with kDesignGlobal waits.
  layout->slot_has_design_behavioral_trigger =
      BuildDesignGlobalBehavioralTriggerBitmap(
          module_plans, *input.design, *input.mir_arena, layout->design);

  auto context = std::make_unique<Context>(
      *input.mir_arena, *input.type_arena, *layout, std::move(llvm_ctx),
      std::move(module), input.diag_ctx, input.source_manager,
      input.origin_lookup, force_two_state);

  // Phase 1: Build specialization inputs (units + layouts + codegen views)
  auto units = BuildSpecCompilationUnits(*input.design);
  auto modidx_to_sched_indices = BuildModuleSchedIndices(*layout);
  auto views =
      BuildSpecCodegenViews(units, *input.design, modidx_to_sched_indices);
  auto spec_slot_infos = BuildSpecSlotInfos(units, *layout, *input.design);

  // Build per-instance local-slot connection trigger facts from the
  // design-global connection trigger bitmap and module plans.
  // This is orchestration-local: the projection from design-global to
  // per-instance local-slot happens here, not on Layout.
  std::vector<std::vector<bool>> instance_connection_triggers(
      module_plans.size());
  for (size_t mi = 0; mi < module_plans.size(); ++mi) {
    const auto& plan = module_plans[mi];
    instance_connection_triggers[mi].assign(plan.slot_count, false);
    for (uint32_t s = 0; s < plan.slot_count; ++s) {
      uint32_t global_slot = plan.design_state_base_slot + s;
      if (global_slot >= layout->slot_has_connection_trigger.size()) {
        throw common::InternalError(
            "CompileDesignProcesses",
            std::format(
                "design-global slot {} out of range for connection trigger "
                "bitmap (size={}, instance={}, local_slot={})",
                global_slot, layout->slot_has_connection_trigger.size(), mi,
                s));
      }
      if (layout->slot_has_connection_trigger[global_slot]) {
        instance_connection_triggers[mi][s] = true;
      }
    }
  }

  // Compute connection-notification masks: conservative union across all
  // instances of each body. Topology-derived codegen compilation decision.
  // Separate from SpecSlotInfo because this is NOT a body semantic fact.
  std::vector<ConnectionNotificationMask> connection_notification_masks(
      units.size());
  for (size_t u = 0; u < units.size(); ++u) {
    auto slot_count = spec_slot_infos[u].SlotCount();
    auto& mask = connection_notification_masks[u];
    mask.required.assign(slot_count, false);
    for (const auto& inst : units[u].instances) {
      auto mi = inst.module_index.value;
      const auto& inst_triggers = instance_connection_triggers.at(mi);
      if (inst_triggers.size() != slot_count) {
        throw common::InternalError(
            "CompileDesignProcesses",
            std::format(
                "instance {} connection_triggers size {} != "
                "spec slot_count {}",
                mi, inst_triggers.size(), slot_count));
      }
      for (uint32_t s = 0; s < slot_count; ++s) {
        if (inst_triggers[s]) {
          mask.required[s] = true;
        }
      }
    }
  }

  auto spec_inputs = BuildCompiledModuleSpecInputs(units, std::move(views));

  // Phase 2: Design-wide init-process monitor registration
  for (mir::ProcessId proc_id : input.design->init_processes) {
    RegisterMonitorInfo(*context, *input.mir_arena, proc_id);
  }

  // Phase 2b: Register deferred assertion site info and thunk metadata.
  // Site info is registered by site ID for enqueue codegen to derive
  // ref binding metadata. Thunk metadata is registered by thunk FunctionId
  // so DefineMirFunction can look it up during body compilation.
  for (uint32_t si = 0; si < input.design->deferred_assertion_sites.size();
       ++si) {
    const auto& site = input.design->deferred_assertion_sites[si];
    context->RegisterDeferredAssertionSiteInfo(
        mir::DeferredAssertionSiteId{si}, &site);
    if (site.pass_action.has_value()) {
      context->RegisterDeferredThunkAction(
          site.pass_action->thunk, &*site.pass_action);
    }
    if (site.fail_action.has_value()) {
      context->RegisterDeferredThunkAction(
          site.fail_action->thunk, &*site.fail_action);
    }
  }

  // Phase 3: Design-global function declare/define only (packages + generated)
  std::vector<mir::FunctionId> global_func_ids;
  std::unordered_set<uint32_t> seen_func_ids;

  for (const auto& element : input.design->elements) {
    const auto* pkg = std::get_if<mir::Package>(&element);
    if (pkg == nullptr) continue;
    for (mir::FunctionId func_id : pkg->functions) {
      if (seen_func_ids.insert(func_id.value).second) {
        global_func_ids.push_back(func_id);
      }
    }
  }
  for (mir::FunctionId func_id : input.design->generated_functions) {
    if (seen_func_ids.insert(func_id.value).second) {
      global_func_ids.push_back(func_id);
    }
  }

  std::vector<std::pair<mir::FunctionId, llvm::Function*>> declared_funcs;
  declared_funcs.reserve(global_func_ids.size());
  for (mir::FunctionId func_id : global_func_ids) {
    auto llvm_func_or_err = DeclareMirFunction(
        *context, func_id, std::format("global_func_{}", func_id.value));
    if (!llvm_func_or_err) return std::unexpected(llvm_func_or_err.error());
    declared_funcs.emplace_back(func_id, *llvm_func_or_err);
  }

  for (const auto& [func_id, llvm_func] : declared_funcs) {
    auto result = DefineMirFunction(*context, func_id, llvm_func);
    if (!result) return std::unexpected(result.error());

    // Register by canonical symbol for DesignFunctionRef resolution
    const auto& func = (*input.mir_arena)[func_id];
    if (func.canonical_symbol) {
      context->RegisterDesignFunction(
          func.canonical_symbol, func_id, llvm_func);
    }
  }

  // Phase 4: Compile each specialization via CompileModuleSpecSession
  // Build body_id -> compiled process functions routing table
  std::unordered_map<uint32_t, std::vector<llvm::Function*>>
      body_to_compiled_funcs;
  std::unordered_map<uint32_t, std::vector<std::optional<ProcessTriggerEntry>>>
      body_to_process_triggers;
  std::vector<WaitSiteEntry> all_wait_sites;
  // Accumulate module-scoped export callees for wrapper emission (D4a).
  // Keyed by (body_id, function_id) to avoid body-local FunctionId collisions.
  // Captured after each CompileModuleSpecSession before user_functions_
  // is overwritten by the next session.
  std::unordered_map<
      mir::ModuleExportCalleeKey, dpi::ModuleExportCalleeInfo,
      mir::ModuleExportCalleeKeyHash>
      module_export_callees;

  for (size_t si = 0; si < spec_inputs.size(); ++si) {
    const auto& spec_input = spec_inputs[si];
    context->SetSpecSlotInfo(&spec_slot_infos[si]);
    context->SetConnectionNotificationMask(&connection_notification_masks[si]);
    auto product =
        CompileModuleSpecSession(*context, *input.design, spec_input);
    if (!product) return std::unexpected(product.error());

    // Capture module export callees from this session's user_functions_
    // before the next session overwrites them.
    if (input.dpi_export_wrappers != nullptr) {
      for (const auto& desc : *input.dpi_export_wrappers) {
        if (desc.target.scope_kind != mir::DpiExportScopeKind::kModule) {
          continue;
        }
        if (desc.target.module_target.body_id != product->body_id) {
          continue;
        }
        auto func_id = desc.target.module_target.function_id;
        if (context->HasUserFunction(func_id)) {
          // Read contract from the body arena (accessible via design, not
          // via context->GetMirArena() which has been restored by ArenaScope).
          const auto& body =
              input.design->module_bodies.at(product->body_id.value);
          const auto& callee = body.arena[func_id];
          module_export_callees[desc.target.module_target] = {
              .llvm_func = context->GetUserFunction(func_id),
              .accepts_decision_owner =
                  callee.abi_contract.accepts_decision_owner,
          };
        }
      }
    }

    body_to_process_triggers.try_emplace(
        product->body_id.value, std::move(product->process_triggers));

    auto [it, inserted] = body_to_compiled_funcs.try_emplace(
        product->body_id.value, std::move(product->process_functions));
    if (!inserted) {
      throw common::InternalError(
          "CompileDesignProcesses",
          std::format(
              "duplicate body_id {} in specialization products",
              product->body_id.value));
    }
    all_wait_sites.insert(
        all_wait_sites.end(),
        std::make_move_iterator(product->wait_sites.begin()),
        std::make_move_iterator(product->wait_sites.end()));
  }

  // Clear per-specialization context state after the compilation loop.
  context->SetSpecSlotInfo(nullptr);
  context->SetConnectionNotificationMask(nullptr);

  // Phase 5: Emit DPI export wrappers (after all callables are declared).
  // Both package and module export wrappers are emitted in one pass.
  if (input.dpi_export_wrappers != nullptr &&
      !input.dpi_export_wrappers->empty()) {
    auto export_result = dpi::EmitDpiExportWrappers(
        *context, *input.dpi_export_wrappers, module_export_callees);
    if (!export_result) return std::unexpected(export_result.error());
  }

  // ArenaScope in CompileModuleSpecSession restores design arena automatically.

  // Phase 5: Descriptor data collection and standalone entrypoints.
  // Module processes get descriptor data instead of per-instance wrappers.
  // Standalone (init/connection) processes keep direct function pointers.
  //
  // process_funcs: standalone call targets plus null module placeholders.
  // Init and connection processes have valid function pointers. Module
  // process entries are nullptr (structural guard -- dispatch for module
  // processes goes through the descriptor table, not this array).
  std::vector<llvm::Function*> process_funcs;
  process_funcs.reserve(layout->scheduled_processes.size());

  // Connection ordinal counter for Phase 5. Tracks the connection
  // realization record being populated during the connection codegen loop.
  uint32_t conn_ordinal = 0;

  size_t num_init = layout->num_init_processes;
  for (size_t i = 0; i < layout->scheduled_processes.size(); ++i) {
    context->SetCurrentProcess(i);

    const auto& bp = layout->scheduled_processes[i];

    // Resolve the correct arena for this process.
    // Init and connection processes use design arena.
    // Module body processes use body arena (resolved via module_index).
    const mir::Arena& proc_arena =
        (bp.module_index.value < module_plans.size())
            ? input.design->module_bodies
                  .at(module_plans[bp.module_index.value].body_id.value)
                  .arena
            : *input.mir_arena;
    const auto& mir_process = proc_arena[bp.process_id];

    // Init and connection processes are standalone (no module binding).
    // Init processes use the init contract (no engine, no dirty tracking).
    // Connection processes are simulation-time and use the simulation contract.
    if (i < num_init || bp.module_index.value == ModuleIndex::kNone) {
      Context::ArenaScope arena_scope(*context, input.mir_arena);
      auto execution_kind = (i < num_init) ? ProcessExecutionKind::kInit
                                           : ProcessExecutionKind::kSimulation;
      auto func_result = GenerateProcessFunction(
          *context, mir_process, std::format("process_{}", i), execution_kind);
      if (!func_result) return std::unexpected(func_result.error());
      process_funcs.push_back(func_result->function);
      all_wait_sites.insert(
          all_wait_sites.end(),
          std::make_move_iterator(func_result->wait_sites.begin()),
          std::make_move_iterator(func_result->wait_sites.end()));
      // Capture connection process trigger facts onto the connection
      // realization record. Phase 5 iterates connection processes in the
      // same order as connection_realization_infos (both follow
      // scheduled_processes[num_init..num_module_process_base)).
      // Template extraction happens in the post-layout pass below.
      if (i >= num_init && i < layout->num_module_process_base) {
        if (conn_ordinal >= layout->connection_realization_infos.size()) {
          throw common::InternalError(
              "CompileDesignProcesses",
              std::format(
                  "connection ordinal {} >= "
                  "connection_realization_infos size {}",
                  conn_ordinal, layout->connection_realization_infos.size()));
        }
        auto& conn_info = layout->connection_realization_infos[conn_ordinal];
        // Verify exact process identity: the scheduled process at this
        // loop position must match the connection realization record.
        if (bp.process_id != conn_info.process_id) {
          throw common::InternalError(
              "CompileDesignProcesses",
              std::format(
                  "connection ordinal {} process_id mismatch: "
                  "scheduled {} != realization {}",
                  conn_ordinal, bp.process_id.value,
                  conn_info.process_id.value));
        }
        if (func_result->process_trigger) {
          const auto& pt = *func_result->process_trigger;
          Layout::ConnectionTriggerResult result;
          result.shape = pt.shape;
          result.triggers.reserve(pt.triggers.size());
          for (const auto& fact : pt.triggers) {
            // Connection trigger signals must be design-global.
            if (fact.signal.scope != mir::SignalRef::Scope::kDesignGlobal) {
              throw common::InternalError(
                  "CompileDesignProcesses",
                  std::format(
                      "connection ordinal {} trigger signal is not "
                      "design-global (scope={}, id={})",
                      conn_ordinal, static_cast<int>(fact.signal.scope),
                      fact.signal.id));
            }
            result.triggers.push_back(
                Layout::ConnectionTriggerFact{
                    .signal = fact.signal,
                    .edge = fact.edge,
                    .has_observed_place = fact.has_observed_place,
                });
          }
          conn_info.trigger = std::move(result);
        }
        ++conn_ordinal;
      }
      continue;
    }

    // Null entry as structural guard: module processes dispatch through
    // frame headers (constructor-owned binding), not through this array.
    process_funcs.push_back(nullptr);
  }

  auto realization = ExtractRealizationData(
      *input.construction, input.design->slots,
      input.design->slot_trace_provenance,
      input.design->slot_trace_string_pool);

  // Build forward-path body compiled functions, parallel to
  // layout->body_realization_infos.
  std::vector<CodegenSession::BodyCompiledFuncs> body_funcs;
  for (const auto& info : layout->body_realization_infos) {
    auto it = body_to_compiled_funcs.find(info.body_id.value);
    if (it == body_to_compiled_funcs.end()) {
      throw common::InternalError(
          "CompileDesignProcesses",
          std::format("no compiled functions for body {}", info.body_id.value));
    }
    if (it->second.size() != info.process_schema_indices.size()) {
      throw common::InternalError(
          "CompileDesignProcesses",
          std::format(
              "body {} compiled function count {} != schema count {}",
              info.body_id.value, it->second.size(),
              info.process_schema_indices.size()));
    }
    body_funcs.push_back(
        CodegenSession::BodyCompiledFuncs{
            .body_id = info.body_id,
            .functions = std::move(it->second),
        });
  }

  // Build per-instance body group index: maps ModuleIndex -> body group.
  // This is retained compile-owned topology summary for the emitted
  // constructor function to emit calls in instance-major order.
  // First, build body_id -> body_group_index mapping.
  std::unordered_map<uint32_t, uint32_t> body_id_to_group;
  for (size_t gi = 0; gi < layout->body_realization_infos.size(); ++gi) {
    body_id_to_group[layout->body_realization_infos[gi].body_id.value] =
        static_cast<uint32_t>(gi);
  }
  // Then walk instances in ModuleIndex order (design element iteration).
  std::vector<uint32_t> instance_body_group;
  {
    uint32_t mi = 0;
    for (const auto& element : input.design->elements) {
      const auto* mod = std::get_if<mir::Module>(&element);
      if (mod == nullptr) continue;
      auto it = body_id_to_group.find(mod->body_id.value);
      if (it == body_id_to_group.end()) {
        throw common::InternalError(
            "CompileDesignProcesses",
            std::format(
                "instance {} body {} not in body_realization_infos", mi,
                mod->body_id.value));
      }
      instance_body_group.push_back(it->second);
      ++mi;
    }
  }

  // Validate: instance_body_group must cover exactly the module instances
  // in the layout (same count as placement.instances / module_plans).
  if (instance_body_group.size() != layout->instance_storage_bases.size()) {
    throw common::InternalError(
        "CompileDesignProcesses",
        std::format(
            "instance_body_group size {} != instance count {}",
            instance_body_group.size(), layout->instance_storage_bases.size()));
  }

  // Post-layout template extraction pass.
  // Sole producer of BodyRealizationInfo.meta/triggers/comb and
  // connection_templates.meta/triggers.
  // Populates descriptor-ready canonical templates from MIR.
  {
    // Build module_index -> body_id mapping (same as in lower loop above).
    std::vector<mir::ModuleBodyId> module_body_ids;
    for (const auto& elem : input.design->elements) {
      if (const auto* mod = std::get_if<mir::Module>(&elem)) {
        module_body_ids.push_back(mod->body_id);
      }
    }

    // Body metadata templates.
    for (auto& info : layout->body_realization_infos) {
      const auto& body = input.design->module_bodies.at(info.body_id.value);
      const auto ordinal_map = BuildBodyProcessOrdinalMap(body);
      info.meta.pool.push_back('\0');

      auto add_pool_string = [&](const std::string& s) -> uint32_t {
        if (s.empty()) return 0;
        auto off = static_cast<uint32_t>(info.meta.pool.size());
        info.meta.pool.insert(info.meta.pool.end(), s.begin(), s.end());
        info.meta.pool.push_back('\0');
        return off;
      };

      auto num_entries =
          static_cast<uint32_t>(info.process_schema_indices.size());
      if (ordinal_map.nonfinal_processes.size() != num_entries) {
        throw common::InternalError(
            "CompileDesignProcesses",
            std::format(
                "body {} non-final process count {} != schema count {}",
                info.body_id.value, ordinal_map.nonfinal_processes.size(),
                num_entries));
      }
      info.meta.entries.resize(num_entries);

      ForEachNonFinalProcess(
          body, ordinal_map,
          [&](uint32_t nonfinal_proc_ordinal, mir::ProcessId /*pid*/,
              const mir::Process& proc) {
            auto kind = MapProcessKind(proc.kind);
            auto loc = ResolveProcessOrigin(
                proc.origin, input.diag_ctx, input.source_manager);

            uint32_t file_off = add_pool_string(loc.file);
            info.meta.entries[nonfinal_proc_ordinal] =
                runtime::ProcessMetaTemplateEntry{
                    .kind_packed = static_cast<uint32_t>(kind),
                    .file_pool_off = file_off,
                    .line = loc.line,
                    .col = loc.col,
                };
          });

      // Decision site metadata: body-wide table shared by all processes.
      //
      // Decision IDs are body-global, allocated from a shared
      // DecisionSiteAllocator during MIR lowering. Each MirDecisionSiteRecord
      // carries its explicit DecisionId. The table size comes from the
      // authoritative body.total_decision_sites (set by the allocator), not
      // from observed records. Every slot must be populated exactly once.
      //
      // First cut: every process in the body gets the same full body-wide
      // table. A future refinement could narrow to the reachable callable
      // graph per process.
      {
        uint32_t table_size = body.total_decision_sites;

        std::vector<runtime::DecisionMetaEntry> body_wide_metas(table_size);
        std::vector<std::string> body_wide_files(table_size);
        std::vector<uint8_t> filled(table_size, 0);

        auto place_record =
            [&](const mir::Process::MirDecisionSiteRecord& rec) {
              auto idx = rec.id.Index();
              if (idx >= table_size) {
                throw common::InternalError(
                    "CompileDesignProcesses",
                    std::format(
                        "decision site id {} out of range in body {} "
                        "(allocator total {})",
                        idx, info.body_id.value, table_size));
              }
              if (filled[idx] != 0) {
                throw common::InternalError(
                    "CompileDesignProcesses",
                    std::format(
                        "decision site id {} duplicated in body {}", idx,
                        info.body_id.value));
              }
              filled[idx] = 1;
              const auto& site = rec.site;
              auto site_loc = ResolveProcessOrigin(
                  site.origin, input.diag_ctx, input.source_manager);
              uint32_t packed =
                  static_cast<uint8_t>(site.qualifier) |
                  (static_cast<uint8_t>(site.kind) << 8) |
                  (static_cast<uint8_t>(site.has_fallback ? 1 : 0) << 16);
              body_wide_metas[idx] = runtime::DecisionMetaEntry{
                  .qualifier_kind_packed = packed,
                  .arm_count = static_cast<uint32_t>(site.arm_count.raw),
                  .line = site_loc.line,
                  .col = site_loc.col,
              };
              body_wide_files[idx] = site_loc.file;
            };

        for (mir::FunctionId fid : body.functions) {
          for (const auto& rec : body.arena[fid].decision_sites) {
            place_record(rec);
          }
        }
        ForEachNonFinalProcess(
            body, ordinal_map,
            [&](uint32_t, mir::ProcessId, const mir::Process& proc) {
              for (const auto& rec : proc.decision_sites) {
                place_record(rec);
              }
            });

        // Every slot in the allocator-defined ID space must be filled.
        for (uint32_t i = 0; i < table_size; ++i) {
          if (filled[i] == 0) {
            throw common::InternalError(
                "CompileDesignProcesses",
                std::format(
                    "decision site id {} missing in body {} "
                    "(allocator total {})",
                    i, info.body_id.value, table_size));
          }
        }

        info.decision_metas.resize(num_entries);
        info.decision_meta_files.resize(num_entries);
        for (uint32_t p = 0; p < num_entries; ++p) {
          info.decision_metas[p] = body_wide_metas;
          info.decision_meta_files[p] = body_wide_files;
        }
      }
    }

    // Connection metadata template.
    layout->connection_templates.meta.pool.push_back('\0');
    auto add_conn_pool_string = [&](const std::string& s) -> uint32_t {
      if (s.empty()) return 0;
      auto off =
          static_cast<uint32_t>(layout->connection_templates.meta.pool.size());
      layout->connection_templates.meta.pool.insert(
          layout->connection_templates.meta.pool.end(), s.begin(), s.end());
      layout->connection_templates.meta.pool.push_back('\0');
      return off;
    };

    for (size_t ci = layout->num_init_processes;
         ci < layout->num_module_process_base; ++ci) {
      const auto& sp = layout->scheduled_processes[ci];
      const mir::Arena& proc_arena =
          (sp.module_index && sp.module_index.value < module_body_ids.size())
              ? input.design->module_bodies
                    .at(module_body_ids[sp.module_index.value].value)
                    .arena
              : *input.mir_arena;
      const auto& proc = proc_arena[sp.process_id];
      auto kind = MapProcessKind(proc.kind);
      auto loc = ResolveProcessOrigin(
          proc.origin, input.diag_ctx, input.source_manager);

      uint32_t file_off = add_conn_pool_string(loc.file);
      layout->connection_templates.meta.entries.push_back(
          runtime::ProcessMetaTemplateEntry{
              .kind_packed = static_cast<uint32_t>(kind),
              .file_pool_off = file_off,
              .line = loc.line,
              .col = loc.col,
          });
    }

    if (layout->connection_templates.meta.entries.size() !=
        layout->connection_realization_infos.size()) {
      throw common::InternalError(
          "CompileDesignProcesses",
          std::format(
              "connection_templates.meta entries {} != "
              "connection_realization_infos {}",
              layout->connection_templates.meta.entries.size(),
              layout->connection_realization_infos.size()));
    }

    // Body trigger templates.
    // body_to_process_triggers[body_id] is indexed by dense non-final
    // body-local ordinal.
    for (auto& info : layout->body_realization_infos) {
      const auto& body = input.design->module_bodies.at(info.body_id.value);
      const auto ordinal_map = BuildBodyProcessOrdinalMap(body);
      auto nonfinal_count =
          static_cast<uint32_t>(ordinal_map.nonfinal_processes.size());

      auto num_procs =
          static_cast<uint32_t>(info.process_schema_indices.size());
      if (num_procs != nonfinal_count) {
        throw common::InternalError(
            "CompileDesignProcesses",
            std::format(
                "body {} schema count {} != non-final process count {}",
                info.body_id.value, num_procs, nonfinal_count));
      }

      info.triggers.proc_ranges.resize(nonfinal_count);
      info.triggers.proc_shapes.resize(
          nonfinal_count,
          static_cast<uint8_t>(runtime::WaitShapeKind::kStatic));
      info.triggers.proc_groupable.resize(
          nonfinal_count, runtime::kProcNotGroupable);

      auto triggers_it = body_to_process_triggers.find(info.body_id.value);
      if (triggers_it == body_to_process_triggers.end()) continue;
      const auto& body_triggers = triggers_it->second;

      if (body_triggers.size() != nonfinal_count) {
        throw common::InternalError(
            "CompileDesignProcesses",
            std::format(
                "body {} trigger vector size {} != non-final process "
                "count {}",
                info.body_id.value, body_triggers.size(), nonfinal_count));
      }

      for (uint32_t nonfinal_proc_ordinal = 0;
           nonfinal_proc_ordinal < nonfinal_count; ++nonfinal_proc_ordinal) {
        if (!body_triggers[nonfinal_proc_ordinal].has_value()) {
          info.triggers.proc_ranges[nonfinal_proc_ordinal] = {
              .start = 0, .count = 0};
          continue;
        }
        const auto& entry = *body_triggers[nonfinal_proc_ordinal];

        auto range_start = static_cast<uint32_t>(info.triggers.entries.size());
        for (const auto& fact : entry.triggers) {
          uint32_t flags = 0;
          if (fact.has_observed_place) {
            flags |= runtime::kTriggerTemplateFlagHasObservedPlace;
          }
          if (fact.signal.scope == mir::SignalRef::Scope::kDesignGlobal) {
            flags |= runtime::kTriggerTemplateFlagDesignGlobal;
          } else if (fact.signal.id >= info.slot_count) {
            throw common::InternalError(
                "CompileDesignProcesses",
                std::format(
                    "body {} proc {} trigger slot_id {} >= slot_count {}",
                    info.body_id.value, nonfinal_proc_ordinal, fact.signal.id,
                    info.slot_count));
          }
          info.triggers.entries.push_back(
              runtime::TriggerTemplateEntry{
                  .slot_id = fact.signal.id,
                  .edge = static_cast<uint32_t>(fact.edge),
                  .flags = flags,
              });
        }
        auto range_count =
            static_cast<uint32_t>(info.triggers.entries.size() - range_start);
        info.triggers.proc_ranges[nonfinal_proc_ordinal] = {
            .start = range_start, .count = range_count};
        info.triggers.proc_shapes[nonfinal_proc_ordinal] =
            static_cast<uint8_t>(entry.shape);

        auto proc_entries =
            std::span(info.triggers.entries).subspan(range_start, range_count);
        info.triggers.proc_groupable[nonfinal_proc_ordinal] =
            IsBodyGroupable(proc_entries, entry.shape)
                ? runtime::kProcGroupable
                : runtime::kProcNotGroupable;
      }
    }

    // Connection trigger template extraction.
    // Source: ConnectionRealizationInfo::trigger, populated during Phase 5
    // connection codegen. Built in one linear pass over connection
    // realization records.
    {
      auto num_conn =
          static_cast<uint32_t>(layout->connection_realization_infos.size());

      if (conn_ordinal != num_conn) {
        throw common::InternalError(
            "CompileDesignProcesses",
            std::format(
                "connection ordinal {} != num_conn {}", conn_ordinal,
                num_conn));
      }

      auto& conn_trig = layout->connection_templates.triggers;
      conn_trig.proc_ranges.resize(num_conn);
      conn_trig.proc_shapes.resize(
          num_conn, static_cast<uint8_t>(runtime::WaitShapeKind::kStatic));
      conn_trig.proc_groupable.resize(num_conn, runtime::kProcNotGroupable);

      for (uint32_t ci = 0; ci < num_conn; ++ci) {
        const auto& conn_info = layout->connection_realization_infos[ci];
        if (!conn_info.trigger) {
          conn_trig.proc_ranges[ci] = {.start = 0, .count = 0};
          continue;
        }
        const auto& result = *conn_info.trigger;

        auto range_start = static_cast<uint32_t>(conn_trig.entries.size());
        for (const auto& fact : result.triggers) {
          uint32_t flags = runtime::kTriggerTemplateFlagDesignGlobal;
          if (fact.has_observed_place) {
            flags |= runtime::kTriggerTemplateFlagHasObservedPlace;
          }
          conn_trig.entries.push_back(
              runtime::TriggerTemplateEntry{
                  .slot_id = fact.signal.id,
                  .edge = static_cast<uint32_t>(fact.edge),
                  .flags = flags,
              });
        }
        auto range_count =
            static_cast<uint32_t>(conn_trig.entries.size() - range_start);
        conn_trig.proc_ranges[ci] = {
            .start = range_start, .count = range_count};
        conn_trig.proc_shapes[ci] = static_cast<uint8_t>(result.shape);

        auto proc_entries =
            std::span(conn_trig.entries).subspan(range_start, range_count);
        conn_trig.proc_groupable[ci] =
            IsBodyGroupable(proc_entries, result.shape)
                ? runtime::kProcGroupable
                : runtime::kProcNotGroupable;
      }
    }

    // Body comb templates.
    // Source: AnalyzeCombKernel (body-relative analysis).
    // Observation resolution for module-local signals uses base_slot +
    // body-relative id. Design-global signals resolve directly with
    // their already-absolute id. Module-local observation resolution
    // requires a valid base_slot; failure to find one is an error.

    // Precompute body_id -> any representative base_slot mapping.
    // Used for observation resolution and canonical sort ordering.
    std::unordered_map<uint32_t, uint32_t> body_base_slots;
    for (const auto& plan : module_plans) {
      body_base_slots.try_emplace(
          plan.body_id.value, plan.design_state_base_slot);
    }

    for (auto& info : layout->body_realization_infos) {
      const auto& body = input.design->module_bodies.at(info.body_id.value);
      const auto ordinal_map = BuildBodyProcessOrdinalMap(body);

      auto base_it = body_base_slots.find(info.body_id.value);
      uint32_t base_slot =
          (base_it != body_base_slots.end()) ? base_it->second : 0;
      bool found_base = (base_it != body_base_slots.end());

      ForEachNonFinalProcess(
          body, ordinal_map,
          [&](uint32_t nonfinal_proc_ordinal, mir::ProcessId /*pid*/,
              const mir::Process& proc) {
            auto result = AnalyzeCombKernel(proc, body.arena);
            if (!result) return;

            auto trigger_start =
                static_cast<uint32_t>(info.comb.entries.size());

            // Per-slot observation merging, keyed by scoped signal identity
            // to avoid merging unrelated observations that share a numeric
            // id across different scopes.
            std::unordered_map<
                ScopedSignalKey, CombSlotAccum, ScopedSignalKeyHash>
                per_slot;

            for (const auto& fact : result->triggers) {
              bool is_global =
                  fact.signal.scope == mir::SignalRef::Scope::kDesignGlobal;

              uint32_t final_global_id = 0;
              if (is_global) {
                final_global_id = fact.signal.id;
              } else {
                if (!found_base) {
                  throw common::InternalError(
                      "CompileDesignProcesses",
                      std::format(
                          "body {} proc {} comb kernel has module-local "
                          "trigger but no instance base_slot found",
                          info.body_id.value, nonfinal_proc_ordinal));
                }
                final_global_id = base_slot + fact.signal.id;
              }

              std::optional<ResolvedObservation> obs;
              if (fact.observed_place) {
                if (is_global) {
                  obs = ResolveObservation(
                      body.arena, layout->design,
                      common::SlotId{fact.signal.id}, *fact.observed_place);
                } else {
                  obs = ResolveObservation(
                      body.arena, layout->design,
                      common::SlotId{final_global_id}, *fact.observed_place);
                }
              }

              ScopedSignalKey key = {
                  .scope = fact.signal.scope, .id = fact.signal.id};
              auto [it, inserted] = per_slot.try_emplace(key);
              auto& accum = it->second;
              if (inserted) {
                accum.is_design_global = is_global;
                accum.final_global_id = final_global_id;
              } else {
                if (accum.is_design_global != is_global) {
                  throw common::InternalError(
                      "CompileDesignProcesses",
                      std::format(
                          "body {} proc {} comb slot ({},{}) scope "
                          "mismatch",
                          info.body_id.value, nonfinal_proc_ordinal,
                          static_cast<int>(key.scope), key.id));
                }
                if (accum.final_global_id != final_global_id) {
                  throw common::InternalError(
                      "CompileDesignProcesses",
                      std::format(
                          "body {} proc {} comb slot ({},{}) "
                          "final_global_id mismatch ({} vs {})",
                          info.body_id.value, nonfinal_proc_ordinal,
                          static_cast<int>(key.scope), key.id,
                          accum.final_global_id, final_global_id));
                }
              }
              if (accum.is_full_slot) continue;
              if (!obs) {
                accum.is_full_slot = true;
                accum.byte_offset = 0;
                accum.byte_size = 0;
              } else if (accum.byte_size == 0 && !accum.is_full_slot) {
                accum.byte_offset = obs->byte_offset;
                accum.byte_size = obs->byte_size;
              } else {
                uint32_t existing_end = accum.byte_offset + accum.byte_size;
                uint32_t new_end = obs->byte_offset + obs->byte_size;
                accum.byte_offset =
                    std::min(accum.byte_offset, obs->byte_offset);
                accum.byte_size =
                    std::max(existing_end, new_end) - accum.byte_offset;
              }
            }

            std::vector<std::pair<ScopedSignalKey, CombSlotAccum>> sorted_slots(
                per_slot.begin(), per_slot.end());
            std::ranges::sort(
                sorted_slots, CompareCombSlotsByFinalObservableOrder);

            for (const auto& [key, accum] : sorted_slots) {
              uint32_t flags = 0;
              uint32_t owner_instance_id = 0;
              uint32_t local_signal_id = 0;
              if (accum.is_design_global) {
                if (accum.final_global_id < layout->num_package_slots) {
                  flags |= runtime::kCombTemplateFlagDesignGlobal;
                } else {
                  flags |= runtime::kCombTemplateFlagCrossInstance;
                  auto owner = ResolveInstanceOwnedFlatSlot(
                      *layout, accum.final_global_id);
                  owner_instance_id = owner.instance_id.value;
                  local_signal_id = owner.local_signal_id.value;
                }
              }
              info.comb.entries.push_back(
                  runtime::CombTemplateEntry{
                      .slot_id = key.id,
                      .byte_offset = accum.is_full_slot ? 0 : accum.byte_offset,
                      .byte_size = accum.is_full_slot ? 0 : accum.byte_size,
                      .flags = flags,
                      .owner_instance_id = owner_instance_id,
                      .local_signal_id = local_signal_id,
                  });
            }

            auto trigger_count =
                static_cast<uint32_t>(info.comb.entries.size() - trigger_start);
            info.comb.kernels.push_back(
                runtime::CombKernelDesc{
                    .proc_within_body = nonfinal_proc_ordinal,
                    .trigger_start = trigger_start,
                    .trigger_count = trigger_count,
                    .has_self_edge =
                        static_cast<uint8_t>(result->has_self_edge),
                });
          });
    }

    // Body observable descriptor templates.
    // Source: design layout (byte offsets, storage specs, ownership),
    // slot trace provenance (local names), type arena (bit widths).
    // Uses the same body_base_slots map already computed for comb templates.
    auto read_trace_pool = [&](uint32_t offset) -> std::string_view {
      const auto& pool = realization.slot_trace_string_pool;
      if (offset >= pool.size()) {
        throw common::InternalError(
            "CompileDesignProcesses",
            std::format(
                "trace pool offset {} out of range (pool size {})", offset,
                pool.size()));
      }
      return {&pool[offset]};
    };

    auto append_to_pool = [](OwnedObservableDescriptorTemplate& tmpl,
                             std::string_view name) -> uint32_t {
      if (name.empty()) return 0;
      auto off = static_cast<uint32_t>(tmpl.pool.size());
      tmpl.pool.insert(tmpl.pool.end(), name.begin(), name.end());
      tmpl.pool.push_back('\0');
      return off;
    };

    auto narrow_u64_to_u32 = [](uint64_t value,
                                std::string_view what) -> uint32_t {
      if (value > std::numeric_limits<uint32_t>::max()) {
        throw common::InternalError(
            "CompileDesignProcesses",
            std::format("{} {} exceeds uint32_t transport limit", what, value));
      }
      return static_cast<uint32_t>(value);
    };

    for (auto& info : layout->body_realization_infos) {
      auto base_it = body_base_slots.find(info.body_id.value);
      if (base_it == body_base_slots.end()) {
        if (info.slot_count == 0) {
          info.observable_descriptors.pool.assign(1, '\0');
          continue;
        }
        throw common::InternalError(
            "CompileDesignProcesses",
            std::format(
                "missing representative base slot for body {} with {} slots",
                info.body_id.value, info.slot_count));
      }
      uint32_t base_slot = base_it->second;

      auto& tmpl = info.observable_descriptors;
      tmpl.pool.push_back('\0');
      tmpl.entries.reserve(info.slot_count);

      if (info.slot_count == 0) continue;

      auto owned_base =
          layout->design.GetStorageBaseForRange(base_slot, info.slot_count);

      // Body-local observable identity: each slot in the body's contiguous
      // range [base_slot, base_slot + slot_count) has a canonical body-local
      // signal id = (gsi - base_slot). This identity is determined by the
      // body's slot allocation in the design layout, which is fixed per
      // specialization. The runtime consumer validates dense population.
      for (uint32_t i = 0; i < info.slot_count; ++i) {
        uint32_t gsi = base_slot + i;
        if (gsi >= layout->design.slots.size()) {
          throw common::InternalError(
              "CompileDesignProcesses",
              std::format(
                  "body {} slot {} (gsi {}) out of range (design slots {})",
                  info.body_id.value, i, gsi, layout->design.slots.size()));
        }

        const ObservableOwnerSlotId owner =
            ObservableOwnerSlotId::Create(layout->design.slots[gsi].value);
        const CanonicalObservableShape shape = ComputeCanonicalObservableShape(
            owner, layout->design, realization, *input.type_arena);
        const ObservableDescriptorShapeFields sf =
            BuildObservableDescriptorShapeFields(shape);

        auto local_name = read_trace_pool(
            realization.slot_trace_provenance[gsi].local_name_str_off);
        uint32_t name_off = append_to_pool(tmpl, local_name);

        // Every body-local slot has body-relative storage (R2 invariant).
        auto body_offset = layout->design.GetBodyOffset(gsi, *owned_base);

        ObservableDescriptorOwnerRefFields refs =
            BuildBodyObservableDescriptorOwnerRefFields(
                owner, base_slot, info.slot_count);

        uint32_t domain = 1;
        uint32_t storage_offset =
            narrow_u64_to_u32(body_offset.value, "body-local byte offset");

        uint32_t body_local_signal_id = gsi - base_slot;
        tmpl.entries.push_back(MakeObservableDescriptorEntry(
            storage_offset, name_off, refs, sf, domain, body_local_signal_id));
      }

      // Producer-side validation: verify all instance-owned descriptors
      // form a complete dense [0, slot_count) range.
      {
        std::vector<uint8_t> seen(info.slot_count, 0);
        for (const auto& entry : tmpl.entries) {
          if (entry.storage_domain != 1) continue;
          if (entry.local_signal_id >= info.slot_count ||
              seen[entry.local_signal_id]++ != 0) {
            throw common::InternalError(
                "CompileDesignProcesses",
                std::format(
                    "body {}: invalid or duplicate local_signal_id {} "
                    "(slot_count {})",
                    info.body_id.value, entry.local_signal_id,
                    info.slot_count));
          }
        }
      }
    }

    // Body init descriptor extraction.
    // Walks each body's slot range to build 4-state patches, owned-handle
    // entries, and param slot templates. All offsets are body-relative.

    // Per-body param slot templates, keyed by body_id for payload lookup.
    BodyParamTemplateMap body_param_templates;

    for (auto& info : layout->body_realization_infos) {
      if (info.slot_count == 0) continue;
      auto base_it = body_base_slots.find(info.body_id.value);
      if (base_it == body_base_slots.end()) {
        throw common::InternalError(
            "CompileDesignProcesses",
            std::format(
                "missing base slot for body {} in init descriptor extraction",
                info.body_id.value));
      }
      uint32_t base_slot = base_it->second;
      auto init_owned_base =
          layout->design.GetStorageBaseForRange(base_slot, info.slot_count);
      if (!init_owned_base.has_value()) continue;

      std::vector<ParamSlotTemplateEntry> param_entries;

      for (uint32_t i = 0; i < info.slot_count; ++i) {
        uint32_t gsi = base_slot + i;

        uint64_t inst_rel_offset =
            layout->design.slot_byte_offsets[gsi] - init_owned_base->value;
        const auto& spec = layout->design.slot_storage_specs[gsi];

        if (realization.slot_kinds[gsi] == mir::SlotKind::kParamConst) {
          param_entries.push_back(
              ParamSlotTemplateEntry{
                  .body_local_slot = i,
                  .slot =
                      runtime::ParamInitSlotEntry{
                          .rel_byte_offset = NarrowToU32(
                              inst_rel_offset, "init param rel offset"),
                          .byte_size = spec.TotalByteSize(),
                      },
              });
        }

        bool is_owned = layout->design.owned_data_offsets[gsi].has_value();
        if (is_owned) {
          uint64_t backing_rel =
              *layout->design.owned_data_offsets[gsi] - init_owned_base->value;
          if (auto root = BuildStorageConstructionRecipeForSlot(
                  spec, layout->design.storage_spec_arena,
                  NarrowToU32(inst_rel_offset, "init rel offset"), true,
                  NarrowToU32(inst_rel_offset, "init handle rel offset"),
                  NarrowToU32(backing_rel, "init backing rel offset"),
                  info.init.storage_recipe, info.init.recipe_child_indices)) {
            info.init.recipe_root_indices.push_back(*root);
          }
        } else {
          if (auto root = BuildStorageConstructionRecipeForSlot(
                  spec, layout->design.storage_spec_arena,
                  NarrowToU32(inst_rel_offset, "init rel offset"), false, 0, 0,
                  info.init.storage_recipe, info.init.recipe_child_indices)) {
            info.init.recipe_root_indices.push_back(*root);
          }
        }
      }

      std::ranges::sort(
          param_entries, {}, &ParamSlotTemplateEntry::body_local_slot);
      info.init.param_slots.reserve(param_entries.size());
      for (const auto& pe : param_entries) {
        info.init.param_slots.push_back(pe.slot);
      }
      body_param_templates[info.body_id] =
          ParamSlotTemplate{.entries = std::move(param_entries)};
    }

    std::vector<uint32_t> design_base_slots;
    design_base_slots.reserve(input.construction->objects.size());
    for (const auto& obj : input.construction->objects) {
      design_base_slots.push_back(obj.design_state_base_slot);
    }

    auto param_payloads = BuildParamPayloads(
        instance_body_group, design_base_slots,
        input.construction->const_blocks, *layout, body_param_templates);

    // Validate parallel structure invariants before building the
    // construction program. These checks catch topology mismatches at
    // compile time rather than letting them survive to runtime.
    {
      auto instance_count = instance_body_group.size();
      if (instance_count != input.construction->instance_table.entries.size()) {
        throw common::InternalError(
            "CompileDesignProcesses",
            std::format(
                "instance_body_group size {} != instance_table size {}",
                instance_count,
                input.construction->instance_table.entries.size()));
      }
      if (instance_count != layout->instance_storage_bases.size()) {
        throw common::InternalError(
            "CompileDesignProcesses",
            std::format(
                "instance_body_group size {} != instance_storage_bases size {}",
                instance_count, layout->instance_storage_bases.size()));
      }
      if (instance_count != param_payloads.size()) {
        throw common::InternalError(
            "CompileDesignProcesses",
            std::format(
                "instance_body_group size {} != param_payloads size {}",
                instance_count, param_payloads.size()));
      }
      auto num_body_groups = body_funcs.size();
      if (num_body_groups != layout->body_realization_infos.size()) {
        throw common::InternalError(
            "CompileDesignProcesses",
            std::format(
                "body_funcs size {} != body_realization_infos size {}",
                num_body_groups, layout->body_realization_infos.size()));
      }
      for (size_t gi = 0; gi < num_body_groups; ++gi) {
        if (body_funcs[gi].body_id !=
            layout->body_realization_infos[gi].body_id) {
          throw common::InternalError(
              "CompileDesignProcesses",
              std::format(
                  "body group {} body_id mismatch: funcs={} vs info={}", gi,
                  body_funcs[gi].body_id.value,
                  layout->body_realization_infos[gi].body_id.value));
        }
      }
      for (size_t mi = 0; mi < instance_count; ++mi) {
        if (instance_body_group[mi] >= num_body_groups) {
          throw common::InternalError(
              "CompileDesignProcesses",
              std::format(
                  "instance {} body_group {} >= num_body_groups {}", mi,
                  instance_body_group[mi], num_body_groups));
        }
      }
    }

    // Build pure-data construction program from the same sources that
    // currently drive per-instance constructor emission. Entries are in
    // strict ModuleIndex order. The runtime relies on this order for
    // instance_id and flat slot-base allocation.
    realization.construction_program = BuildConstructionProgram(
        instance_body_group, *layout, input.construction->instance_table,
        param_payloads);

    // Validate: construction program entries match lowering domain.
    for (size_t i = 0; i < realization.construction_program.entries.size();
         ++i) {
      const auto& e = realization.construction_program.entries[i];
      if (e.body_group >= layout->body_realization_infos.size()) {
        throw common::InternalError(
            "CompileDesignProcesses",
            std::format(
                "construction entry {} body_group {} >= "
                "body_realization_infos size {}",
                i, e.body_group, layout->body_realization_infos.size()));
      }
    }

    // Package/global init descriptor extraction.
    {
      auto& pkg_init = layout->package_init_descriptor;
      uint32_t num_pkg = layout->num_package_slots;
      const auto& spec_arena = layout->design.storage_spec_arena;

      for (uint32_t gsi = 0; gsi < num_pkg; ++gsi) {
        if (gsi >= layout->design.slots.size()) break;
        const auto& spec = layout->design.slot_storage_specs[gsi];
        uint64_t abs_offset = layout->design.slot_byte_offsets[gsi];

        bool is_owned = layout->design.owned_data_offsets[gsi].has_value();
        if (is_owned) {
          uint64_t backing_abs = *layout->design.owned_data_offsets[gsi];
          if (auto root = BuildStorageConstructionRecipeForSlot(
                  spec, spec_arena,
                  NarrowToU32(abs_offset, "pkg init rel offset"), true,
                  NarrowToU32(abs_offset, "pkg init handle offset"),
                  NarrowToU32(backing_abs, "pkg init backing offset"),
                  pkg_init.storage_recipe, pkg_init.recipe_child_indices)) {
            pkg_init.recipe_root_indices.push_back(*root);
          }
        } else {
          if (auto root = BuildStorageConstructionRecipeForSlot(
                  spec, spec_arena,
                  NarrowToU32(abs_offset, "pkg init rel offset"), false, 0, 0,
                  pkg_init.storage_recipe, pkg_init.recipe_child_indices)) {
            pkg_init.recipe_root_indices.push_back(*root);
          }
        }
      }
    }

    // Package/global observable descriptor template.
    {
      auto& pkg = layout->package_observable_descriptors;
      pkg.pool.push_back('\0');
      uint32_t num_pkg = layout->num_package_slots;
      pkg.entries.reserve(num_pkg);

      for (uint32_t gsi = 0; gsi < num_pkg; ++gsi) {
        if (gsi >= layout->design.slots.size()) break;

        const ObservableOwnerSlotId owner =
            ObservableOwnerSlotId::Create(layout->design.slots[gsi].value);
        const CanonicalObservableShape shape = ComputeCanonicalObservableShape(
            owner, layout->design, realization, *input.type_arena);
        const ObservableDescriptorShapeFields sf =
            BuildObservableDescriptorShapeFields(shape);

        const auto& prov = realization.slot_trace_provenance[gsi];
        auto local_name = read_trace_pool(prov.local_name_str_off);
        std::string qualified_name;
        if (prov.scope_kind == mir::SlotScopeKind::kPackage) {
          auto pkg_name = read_trace_pool(prov.scope_ref);
          qualified_name = std::format("{}.{}", pkg_name, local_name);
        } else {
          qualified_name = std::string(local_name);
        }
        uint32_t name_off = append_to_pool(pkg, qualified_name);

        uint32_t storage_offset = narrow_u64_to_u32(
            layout->design.slot_byte_offsets[gsi],
            "package/global byte offset");
        const ObservableDescriptorOwnerRefFields refs =
            BuildPackageObservableDescriptorOwnerRefFields(owner);

        pkg.entries.push_back(MakeObservableDescriptorEntry(
            storage_offset, name_off, refs, sf, 0));
      }
    }
  }

  return CodegenSession{
      .layout = std::move(layout),
      .context = std::move(context),
      .realization = std::move(realization),
      .process_funcs = std::move(process_funcs),
      .wait_sites = std::move(all_wait_sites),
      .slot_info = std::move(slot_info),
      .num_init_processes = num_init,
      .body_compiled_funcs = std::move(body_funcs),
  };
}

auto FinalizeModule(CodegenSession session, LoweringReport report)
    -> LoweringResult {
  auto [result_ctx, result_mod] = session.context->TakeOwnership();
  return LoweringResult{
      .context = std::move(result_ctx),
      .module = std::move(result_mod),
      .report = std::move(report),
  };
}

void EmitVariableInspection(
    Context& context, const InspectionPlan& plan,
    const std::vector<SlotInfo>& slots, llvm::Value* design_state,
    llvm::Value* abi_ptr) {
  if (plan.IsEmpty()) {
    return;
  }

  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);
  auto* i1_ty = llvm::Type::getInt1Ty(llvm_ctx);
  auto* i8_ty = llvm::Type::getInt8Ty(llvm_ctx);

  auto emit_register = [&](llvm::Value* slot_ptr, std::string_view name,
                           common::SlotId slot_id) {
    const auto& type_info = slots[slot_id.value].type_info;
    auto* name_ptr = builder.CreateGlobalStringPtr(name);
    auto* kind_val =
        llvm::ConstantInt::get(i32_ty, static_cast<int32_t>(type_info.kind));
    auto* width_val = llvm::ConstantInt::get(i32_ty, type_info.width);
    auto* signed_val =
        llvm::ConstantInt::get(i1_ty, type_info.is_signed ? 1 : 0);
    auto* four_state_val =
        llvm::ConstantInt::get(i1_ty, type_info.is_four_state ? 1 : 0);
    builder.CreateCall(
        context.GetLyraRegisterVar(),
        {name_ptr, slot_ptr, kind_val, width_val, signed_val, four_state_val});
  };

  // Design-global variables: always emitted unconditionally.
  for (const auto& var : plan.globals) {
    auto* addr = builder.CreateGEP(
        i8_ty, design_state, builder.getInt64(var.placement.abs_off.value),
        "var_ptr");
    emit_register(addr, var.name, var.slot_id);
  }

  // Instance-owned variables: guarded by runtime abi_ptr null check.
  if (!plan.instance_owned.empty()) {
    auto* fn = builder.GetInsertBlock()->getParent();
    auto* with_abi_bb = llvm::BasicBlock::Create(llvm_ctx, "inspect.inst", fn);
    auto* skip_bb = llvm::BasicBlock::Create(llvm_ctx, "inspect.inst.done", fn);

    auto* ptr_ty = llvm::PointerType::getUnqual(llvm_ctx);
    auto* null_ptr_val =
        llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptr_ty));
    auto* has_abi = builder.CreateICmpNE(abi_ptr, null_ptr_val, "has_abi");
    builder.CreateCondBr(has_abi, with_abi_bb, skip_bb);

    builder.SetInsertPoint(with_abi_bb);
    for (const auto& var : plan.instance_owned) {
      auto* inst = EmitLoadAbiInstancePtr(
          context, abi_ptr, var.placement.owner_instance_id);
      auto* addr =
          EmitInstanceOwnedByteAddress(context, inst, var.placement.rel_off);
      emit_register(addr, var.name, var.slot_id);
    }
    builder.CreateBr(skip_bb);
    builder.SetInsertPoint(skip_bb);
  }

  builder.CreateCall(context.GetLyraSnapshotVars());
}

void EmitTimeReport(Context& context) {
  context.GetBuilder().CreateCall(context.GetLyraReportTime());
}

auto DumpLlvmIr(const LoweringResult& result) -> std::string {
  std::string ir;
  llvm::raw_string_ostream stream(ir);
  result.module->print(stream, nullptr);
  return ir;
}

}  // namespace lyra::lowering::mir_to_llvm
