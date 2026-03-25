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
#include "lyra/llvm_backend/connection_kernel_collection.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/forwarding_analysis.hpp"
#include "lyra/llvm_backend/forwarding_map.hpp"
#include "lyra/llvm_backend/layout/layout.hpp"
#include "lyra/llvm_backend/process.hpp"
#include "lyra/llvm_backend/process_meta_utils.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/module.hpp"
#include "lyra/mir/placement.hpp"
#include "lyra/mir/routine.hpp"
#include "lyra/mir/statement.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

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

auto BuildSpecCompilationUnits(const mir::Design& design)
    -> std::vector<SpecCompilationUnit> {
  std::vector<SpecCompilationUnit> units;
  std::unordered_map<uint32_t, size_t> body_to_unit;

  uint32_t module_idx = 0;
  for (const auto& element : design.elements) {
    const auto* mod = std::get_if<mir::Module>(&element);
    if (mod == nullptr) continue;

    auto body_id = mod->body_id;
    const auto& placement =
        mir::GetInstancePlacement(design.placement, module_idx);

    SpecInstanceBinding binding{
        .module_index = ModuleIndex{module_idx},
        .signal_id_offset = placement.design_state_base_slot,
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

    // Count non-final processes (resolve from body arena)
    const auto& body_arena = design.module_bodies.at(unit.body_id.value).arena;
    uint32_t num_nonfinal = 0;
    for (mir::ProcessId proc_id : unit.processes) {
      if (body_arena[proc_id].kind != mir::ProcessKind::kFinal) ++num_nonfinal;
    }
    if (num_nonfinal == 0) continue;

    // Use first instance as representative for scheduled-process indexing
    ModuleIndex rep_module_index = unit.instances[0].module_index;
    auto sched_it = modidx_to_sched_indices.find(rep_module_index.value);
    if (sched_it == modidx_to_sched_indices.end()) {
      throw common::InternalError(
          "BuildSpecCodegenViews",
          std::format(
              "body {} has {} non-final processes but representative "
              "module_index {} has no scheduled processes",
              unit.body_id.value, num_nonfinal, rep_module_index.value));
    }
    const auto& sched_indices = sched_it->second;
    if (sched_indices.size() != num_nonfinal) {
      throw common::InternalError(
          "BuildSpecCodegenViews",
          std::format(
              "body {} has {} non-final processes but representative "
              "module_index {} has {} scheduled processes",
              unit.body_id.value, num_nonfinal, rep_module_index.value,
              sched_indices.size()));
    }

    uint32_t local_nonfinal = 0;
    for (mir::ProcessId proc_id : unit.processes) {
      if (body_arena[proc_id].kind == mir::ProcessKind::kFinal) continue;

      views[u].processes.push_back(
          SpecProcessView{
              .local_nonfinal_proc_index = local_nonfinal,
              .layout_process_index = sched_indices[local_nonfinal],
              .process_id = proc_id,
              .func_name = std::format(
                  "body_{}_proc_{}", unit.body_id.value, local_nonfinal),
          });
      ++local_nonfinal;
    }
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
    const std::vector<uint32_t>& module_base_slots,
    std::span<const mir::SlotDesc> design_slots) -> std::vector<SpecSlotInfo> {
  std::vector<SpecSlotInfo> result;
  result.reserve(units.size());
  for (const auto& unit : units) {
    if (unit.instances.empty()) {
      throw common::InternalError(
          "BuildSpecSlotInfos", "specialization unit has no instances");
    }
    SpecSlotInfo info;
    auto rep_idx = unit.instances[0].module_index;
    uint32_t rep_base_slot = module_base_slots[rep_idx.value];
    auto slot_count =
        static_cast<uint32_t>(layout.GetInstanceRelByteOffsets(rep_idx).size());
    info.inline_offsets.reserve(slot_count);
    info.shapes.reserve(slot_count);
    info.representative_design_slots.reserve(slot_count);
    uint64_t base_offset = layout.GetInstanceBaseByteOffset(rep_idx);
    for (uint32_t s = 0; s < slot_count; ++s) {
      uint32_t design_slot_row = rep_base_slot + s;
      auto slot_id = layout.design.slots[design_slot_row];
      uint64_t abs_offset = layout.design.GetStorageByteOffset(slot_id);
      if (abs_offset < base_offset) {
        throw common::InternalError(
            "BuildSpecSlotInfos", std::format(
                                      "slot {} abs_offset {} < base_offset {}",
                                      slot_id.value, abs_offset, base_offset));
      }
      info.inline_offsets.push_back(abs_offset - base_offset);
      info.shapes.push_back(design_slots[design_slot_row].storage_shape);
      info.representative_design_slots.push_back(slot_id.value);
    }

    // Verify all instances in this specialization agree on slot count and
    // storage shape.
    for (size_t i = 1; i < unit.instances.size(); ++i) {
      auto inst_slot_count = static_cast<uint32_t>(
          layout.GetInstanceRelByteOffsets(unit.instances[i].module_index)
              .size());
      if (inst_slot_count != slot_count) {
        throw common::InternalError(
            "BuildSpecSlotInfos",
            std::format(
                "slot count mismatch: representative has {} but instance {} "
                "has {}",
                slot_count, unit.instances[i].module_index.value,
                inst_slot_count));
      }
      uint32_t inst_base =
          module_base_slots[unit.instances[i].module_index.value];
      for (uint32_t s = 0; s < slot_count; ++s) {
        auto inst_shape = design_slots[inst_base + s].storage_shape;
        if (inst_shape != info.shapes[s]) {
          throw common::InternalError(
              "BuildSpecSlotInfos",
              std::format(
                  "storage shape mismatch at body-local slot {}: "
                  "representative={} instance={}",
                  s, static_cast<int>(info.shapes[s]),
                  static_cast<int>(inst_shape)));
        }
      }
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
  // Set the body arena as the current arena for this compilation scope.
  // ArenaScope restores the previous arena on exit.
  const auto& body = design.module_bodies.at(input.body_id.value);
  Context::ArenaScope arena_scope(context, &body.arena);

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
        func_id, {.spec_slot_info = context.GetSpecSlotInfo()});
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
    const mir::PlacementMap& placement, std::span<const mir::SlotDesc> slots,
    const mir::InstanceTable& instance_table,
    std::span<const mir::SlotTraceProvenance> slot_trace_provenance,
    std::span<const char> slot_trace_string_pool) -> RealizationData {
  RealizationData realization;

  if (placement.instances.size() != placement.const_blocks.size()) {
    throw common::InternalError(
        "ExtractRealizationData",
        std::format(
            "placement instances/const_blocks size mismatch: {} vs {}",
            placement.instances.size(), placement.const_blocks.size()));
  }
  realization.param_inits.reserve(placement.const_blocks.size());
  for (size_t i = 0; i < placement.const_blocks.size(); ++i) {
    const auto& inst = placement.instances[i];
    const auto& const_block = placement.const_blocks[i];
    auto& resolved = realization.param_inits.emplace_back();
    resolved.reserve(const_block.slot_inits.size());
    for (const auto& init : const_block.slot_inits) {
      uint32_t abs_slot = inst.design_state_base_slot + init.body_local_slot;
      if (abs_slot >= slots.size()) {
        throw common::InternalError(
            "ExtractRealizationData",
            std::format(
                "param init abs_slot {} out of range (slots.size() = {})",
                abs_slot, slots.size()));
      }
      resolved.push_back(
          ResolvedParamInit{
              .slot_id = abs_slot,
              .type_id = slots[abs_slot].type,
              .value = init.value,
          });
    }
  }

  realization.slot_types.reserve(slots.size());
  realization.slot_kinds.reserve(slots.size());
  for (const auto& slot : slots) {
    realization.slot_types.push_back(slot.type);
    realization.slot_kinds.push_back(slot.kind);
  }

  realization.instance_paths.reserve(instance_table.entries.size());
  for (const auto& entry : instance_table.entries) {
    realization.instance_paths.push_back(entry.full_path);
  }

  realization.slot_trace_provenance.assign(
      slot_trace_provenance.begin(), slot_trace_provenance.end());
  realization.slot_trace_string_pool.assign(
      slot_trace_string_pool.begin(), slot_trace_string_pool.end());

  return realization;
}

auto CompileDesignProcesses(const LoweringInput& input)
    -> Result<CodegenSession> {
  // Phase 0: Backend/session setup
  auto llvm_ctx = std::make_unique<llvm::LLVMContext>();
  auto module = std::make_unique<llvm::Module>("lyra_module", *llvm_ctx);

  SetHostDataLayout(*module);

  bool force_two_state = input.force_two_state;

  auto slot_info =
      BuildSlotInfo(input.design->slots, *input.type_arena, force_two_state);

  // Pre-forwarding layout: used by connection collection for observation
  // resolution (needs slot specs, not byte offsets).
  ForwardingMap empty_forwarding;
  auto pre_forwarding_layout = BuildDesignLayout(
      slot_info, *input.type_arena, module->getDataLayout(), force_two_state,
      empty_forwarding);

  // Extract narrow layout-planning inputs from design.
  // module_plans and module_base_slots are produced once and consumed by
  // BuildLayout and wrapper generation respectively.
  //
  // Contract: module_plans[i] corresponds to the i-th Module element in
  // design.elements (skipping non-Module variants). This ordering matches
  // placement.instances[i], which is how ModuleIndex is assigned during
  // MIR construction. BuildLayout and wrapper generation depend on this.
  std::vector<LayoutModulePlan> module_plans;
  std::vector<uint32_t> module_base_slots;
  {
    uint32_t module_idx = 0;
    for (const auto& element : input.design->elements) {
      const auto* mod = std::get_if<mir::Module>(&element);
      if (mod == nullptr) continue;
      const auto& body = input.design->module_bodies.at(mod->body_id.value);
      const auto& placement =
          mir::GetInstancePlacement(input.design->placement, module_idx);
      module_plans.push_back(
          LayoutModulePlan{
              .body_processes = body.processes,
              .body_id = mod->body_id,
              .design_state_base_slot = placement.design_state_base_slot,
              .slot_count = placement.slot_count,
          });
      module_base_slots.push_back(placement.design_state_base_slot);
      ++module_idx;
    }
    if (module_idx != input.design->placement.instances.size()) {
      throw common::InternalError(
          "CompileDesignProcesses",
          std::format(
              "module count {} from elements does not match placement "
              "instance count {}",
              module_idx, input.design->placement.instances.size()));
    }
  }

  // Connection-kernel collection uses pre-forwarding layout for trigger
  // observation resolution. Only slot_storage_specs, storage_spec_arena,
  // and slot_to_index are consumed.
  // No later phase may re-collect connection kernels.
  auto connection_collection = CollectConnectionKernels(
      input.design->connection_processes, *input.mir_arena,
      SlotSpecView{
          .slot_to_index = pre_forwarding_layout.slot_to_index,
          .slot_storage_specs = pre_forwarding_layout.slot_storage_specs,
          .storage_spec_arena = pre_forwarding_layout.storage_spec_arena,
      });

  // Non-connection trigger slot summary for forwarding exclusion.
  auto trigger_summary =
      CollectTriggerSlotSummary(module_plans, *input.design, *input.mir_arena);

  // Canonical forwarding analysis. No later phase may add new candidates.
  auto num_design_slots = static_cast<uint32_t>(input.design->slots.size());
  auto forwarding_map = BuildForwardingMap(
      connection_collection.kernel_entries, trigger_summary, num_design_slots);

  // Rebuild connections through canonical forwarding resolution.
  auto canonicalized_connections = RebuildCanonicalConnections(
      std::move(connection_collection.kernel_entries), forwarding_map);

  // Structural verification: canonicalized connections must not contain
  // any forwarded alias slot in any slot-bearing field.
  VerifyCanonicalizedConnectionObservations(
      forwarding_map, canonicalized_connections);

  // Build final layout with forwarding-aware storage ownership.
  auto design_layout = BuildDesignLayout(
      slot_info, *input.type_arena, module->getDataLayout(), force_two_state,
      forwarding_map);

  auto layout = std::make_unique<Layout>(BuildLayout(
      input.design->init_processes, std::move(canonicalized_connections),
      std::move(connection_collection.non_kernelized_processes), module_plans,
      *input.design, *input.mir_arena, *input.type_arena,
      std::move(design_layout), *llvm_ctx, module->getDataLayout(),
      force_two_state));

  auto context = std::make_unique<Context>(
      *input.mir_arena, *input.type_arena, *layout, std::move(llvm_ctx),
      std::move(module), input.diag_ctx, force_two_state);

  // Phase 1: Build specialization inputs (units + layouts + codegen views)
  auto units = BuildSpecCompilationUnits(*input.design);
  auto modidx_to_sched_indices = BuildModuleSchedIndices(*layout);
  auto views =
      BuildSpecCodegenViews(units, *input.design, modidx_to_sched_indices);
  auto spec_slot_infos = BuildSpecSlotInfos(
      units, *layout, module_base_slots, input.design->slots);
  auto spec_inputs = BuildCompiledModuleSpecInputs(units, std::move(views));

  // Phase 2: Design-wide init-process monitor registration
  for (mir::ProcessId proc_id : input.design->init_processes) {
    RegisterMonitorInfo(*context, *input.mir_arena, proc_id);
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

  for (size_t si = 0; si < spec_inputs.size(); ++si) {
    const auto& spec_input = spec_inputs[si];
    context->SetSpecSlotInfo(&spec_slot_infos[si]);
    auto product =
        CompileModuleSpecSession(*context, *input.design, spec_input);
    if (!product) return std::unexpected(product.error());

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
      input.design->placement, input.design->slots,
      input.design->instance_table, input.design->slot_trace_provenance,
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
  if (instance_body_group.size() != layout->instance_base_byte_offsets.size()) {
    throw common::InternalError(
        "CompileDesignProcesses",
        std::format(
            "instance_body_group size {} != instance count {}",
            instance_body_group.size(),
            layout->instance_base_byte_offsets.size()));
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
      info.meta.entries.resize(num_entries);

      for (uint32_t pwb = 0; pwb < num_entries; ++pwb) {
        if (pwb >= body.processes.size()) {
          throw common::InternalError(
              "CompileDesignProcesses",
              std::format(
                  "body {} proc_within_body {} >= body process count {}",
                  info.body_id.value, pwb, body.processes.size()));
        }
        const auto& proc = body.arena[body.processes[pwb]];
        if (proc.kind == mir::ProcessKind::kFinal) {
          throw common::InternalError(
              "CompileDesignProcesses",
              std::format(
                  "body {} proc_within_body {} is kFinal "
                  "(metadata template only covers non-final processes)",
                  info.body_id.value, pwb));
        }
        auto kind = MapProcessKind(proc.kind);
        auto loc = ResolveProcessOrigin(
            proc.origin, input.diag_ctx, input.source_manager);

        uint32_t file_off = add_pool_string(loc.file);
        info.meta.entries[pwb] = runtime::ProcessMetaTemplateEntry{
            .kind_packed = static_cast<uint32_t>(kind),
            .file_pool_off = file_off,
            .line = loc.line,
            .col = loc.col,
        };
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
    // Source: body_to_process_triggers (body-shaped, body-relative signals).
    for (auto& info : layout->body_realization_infos) {
      auto triggers_it = body_to_process_triggers.find(info.body_id.value);
      auto num_procs =
          static_cast<uint32_t>(info.process_schema_indices.size());
      info.triggers.proc_ranges.resize(num_procs);
      info.triggers.proc_shapes.resize(
          num_procs, static_cast<uint8_t>(runtime::WaitShapeKind::kStatic));
      info.triggers.proc_groupable.resize(
          num_procs, runtime::kProcNotGroupable);

      if (triggers_it == body_to_process_triggers.end()) continue;
      const auto& body_triggers = triggers_it->second;

      for (uint32_t pwb = 0; pwb < num_procs; ++pwb) {
        if (pwb >= body_triggers.size() || !body_triggers[pwb].has_value()) {
          info.triggers.proc_ranges[pwb] = {.start = 0, .count = 0};
          continue;
        }
        const auto& entry = *body_triggers[pwb];

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
                    info.body_id.value, pwb, fact.signal.id, info.slot_count));
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
        info.triggers.proc_ranges[pwb] = {
            .start = range_start, .count = range_count};
        info.triggers.proc_shapes[pwb] = static_cast<uint8_t>(entry.shape);

        auto proc_entries =
            std::span(info.triggers.entries).subspan(range_start, range_count);
        info.triggers.proc_groupable[pwb] =
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

      auto base_it = body_base_slots.find(info.body_id.value);
      uint32_t base_slot =
          (base_it != body_base_slots.end()) ? base_it->second : 0;
      bool found_base = (base_it != body_base_slots.end());

      auto num_procs =
          static_cast<uint32_t>(info.process_schema_indices.size());
      for (uint32_t pwb = 0; pwb < num_procs; ++pwb) {
        if (pwb >= body.processes.size()) continue;
        const auto& proc = body.arena[body.processes[pwb]];
        auto result = AnalyzeCombKernel(proc, body.arena);
        if (!result) continue;

        auto trigger_start = static_cast<uint32_t>(info.comb.entries.size());

        // Per-slot observation merging, keyed by scoped signal identity
        // to avoid merging unrelated observations that share a numeric id
        // across different scopes.
        std::unordered_map<ScopedSignalKey, CombSlotAccum, ScopedSignalKeyHash>
            per_slot;

        for (const auto& fact : result->triggers) {
          bool is_global =
              fact.signal.scope == mir::SignalRef::Scope::kDesignGlobal;

          // Compute final design-global slot id for sort ordering.
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
                      info.body_id.value, pwb));
            }
            final_global_id = base_slot + fact.signal.id;
          }

          std::optional<ResolvedObservation> obs;
          if (fact.observed_place) {
            if (is_global) {
              obs = ResolveObservation(
                  body.arena, layout->design, mir::SlotId{fact.signal.id},
                  *fact.observed_place);
            } else {
              // found_base is guaranteed true here (checked above).
              obs = ResolveObservation(
                  body.arena, layout->design, mir::SlotId{final_global_id},
                  *fact.observed_place);
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
            // Scope consistency: the same scoped key must always have
            // the same design-global classification.
            if (accum.is_design_global != is_global) {
              throw common::InternalError(
                  "CompileDesignProcesses",
                  std::format(
                      "body {} proc {} comb slot ({},{}) scope mismatch",
                      info.body_id.value, pwb, static_cast<int>(key.scope),
                      key.id));
            }
            if (accum.final_global_id != final_global_id) {
              throw common::InternalError(
                  "CompileDesignProcesses",
                  std::format(
                      "body {} proc {} comb slot ({},{}) "
                      "final_global_id mismatch ({} vs {})",
                      info.body_id.value, pwb, static_cast<int>(key.scope),
                      key.id, accum.final_global_id, final_global_id));
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
            accum.byte_offset = std::min(accum.byte_offset, obs->byte_offset);
            accum.byte_size =
                std::max(existing_end, new_end) - accum.byte_offset;
          }
        }

        std::vector<std::pair<ScopedSignalKey, CombSlotAccum>> sorted_slots(
            per_slot.begin(), per_slot.end());
        std::ranges::sort(sorted_slots, CompareCombSlotsByFinalObservableOrder);

        for (const auto& [key, accum] : sorted_slots) {
          uint32_t flags = 0;
          if (accum.is_design_global) {
            flags |= runtime::kCombTemplateFlagDesignGlobal;
          }
          info.comb.entries.push_back(
              runtime::CombTemplateEntry{
                  .slot_id = key.id,
                  .byte_offset = accum.is_full_slot ? 0 : accum.byte_offset,
                  .byte_size = accum.is_full_slot ? 0 : accum.byte_size,
                  .flags = flags,
              });
        }

        auto trigger_count =
            static_cast<uint32_t>(info.comb.entries.size() - trigger_start);
        info.comb.kernels.push_back(
            runtime::CombKernelDesc{
                .proc_within_body = pwb,
                .trigger_start = trigger_start,
                .trigger_count = trigger_count,
                .has_self_edge = static_cast<uint8_t>(result->has_self_edge),
            });
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
      .instance_body_group = std::move(instance_body_group),
  };
}

auto FinalizeModule(CodegenSession session) -> LoweringResult {
  auto [result_ctx, result_mod] = session.context->TakeOwnership();
  return LoweringResult{
      .context = std::move(result_ctx),
      .module = std::move(result_mod),
  };
}

void EmitVariableInspection(
    Context& context, const std::vector<VariableInfo>& variables,
    const std::vector<SlotInfo>& slots, llvm::Value* design_state) {
  if (variables.empty()) {
    return;
  }

  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);
  auto* i1_ty = llvm::Type::getInt1Ty(llvm_ctx);
  auto* i8_ty = llvm::Type::getInt8Ty(llvm_ctx);

  for (const auto& var : variables) {
    if (var.slot_id >= slots.size()) {
      continue;
    }

    auto slot_id = mir::SlotId{static_cast<uint32_t>(var.slot_id)};
    uint64_t offset = context.GetDesignSlotByteOffset(slot_id);
    auto* slot_ptr = builder.CreateGEP(
        i8_ty, design_state, builder.getInt64(offset), "var_ptr");

    const auto& type_info = slots[var.slot_id].type_info;

    auto* name_ptr = builder.CreateGlobalStringPtr(var.name);
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
