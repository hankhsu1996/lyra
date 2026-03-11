#include "lyra/llvm_backend/lower.hpp"

#include <cstddef>
#include <cstdint>
#include <expected>
#include <format>
#include <iterator>
#include <memory>
#include <mutex>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include <llvm/ADT/StringMap.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
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
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/layout/layout.hpp"
#include "lyra/llvm_backend/process.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/module.hpp"
#include "lyra/mir/placement.hpp"
#include "lyra/mir/routine.hpp"
#include "lyra/mir/statement.hpp"
#include "lyra/runtime/feature_flags.hpp"

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
          monitor->check_thunk, std::move(mon_layout));

      Context::MonitorSetupInfo setup_info{
          .check_thunk = monitor->check_thunk,
      };
      context.RegisterMonitorSetupInfo(
          monitor->setup_thunk, std::move(setup_info));
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
        .base_slot_id = placement.design_state_base_slot,
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
    const std::vector<SpecCompilationUnit>& units, const mir::Arena& arena,
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

    // Count non-final processes
    uint32_t num_nonfinal = 0;
    for (mir::ProcessId proc_id : unit.processes) {
      if (arena[proc_id].kind != mir::ProcessKind::kFinal) ++num_nonfinal;
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
      if (arena[proc_id].kind == mir::ProcessKind::kFinal) continue;

      views[u].processes.push_back(
          SpecProcessView{
              .local_nonfinal_proc_index = local_nonfinal,
              .layout_process_index = sched_indices[local_nonfinal],
              .process_id = proc_id,
              .representative_module_index = rep_module_index,
              .func_name = std::format(
                  "body_{}_proc_{}", unit.body_id.value, local_nonfinal),
          });
      ++local_nonfinal;
    }
  }

  return views;
}

auto BuildSpecLayouts(
    const std::vector<SpecCompilationUnit>& units, const Layout& layout)
    -> std::vector<SpecLayout> {
  std::vector<SpecLayout> layouts;
  layouts.reserve(units.size());
  for (const auto& unit : units) {
    if (unit.instances.empty()) {
      throw common::InternalError(
          "BuildSpecLayouts",
          std::format(
              "specialization unit for body {} has no instances",
              unit.body_id.value));
    }
    layouts.push_back(
        SpecLayout{
            .rel_byte_offsets = layout.GetBodyRelByteOffsets(unit.body_id),
        });
  }
  return layouts;
}

auto BuildCompiledModuleSpecInputs(
    const std::vector<SpecCompilationUnit>& units,
    std::vector<SpecLayout> layouts, std::vector<SpecCodegenView> views)
    -> std::vector<CompiledModuleSpecInput> {
  std::vector<CompiledModuleSpecInput> inputs;
  inputs.reserve(units.size());
  for (size_t i = 0; i < units.size(); ++i) {
    inputs.push_back(
        CompiledModuleSpecInput{
            .body_id = units[i].body_id,
            .processes = units[i].processes,
            .functions = units[i].functions,
            .layout = std::move(layouts[i]),
            .view = std::move(views[i]),
        });
  }
  return inputs;
}

}  // namespace

auto CompileModuleSpecSession(
    Context& context, const mir::Arena& arena,
    const CompiledModuleSpecInput& input) -> Result<CompiledModuleSpec> {
  // Step 1: Register monitor info for body processes
  for (mir::ProcessId proc_id : input.processes) {
    RegisterMonitorInfo(context, arena, proc_id);
  }

  // Step 2: Register module-scoped function lowering metadata.
  // Once per function, not per instance -- rel_byte_offsets is
  // specialization-owned and shared across all instances.
  for (mir::FunctionId func_id : input.functions) {
    const auto& func = arena[func_id];
    if (func.thunk_kind == mir::ThunkKind::kNone) {
      context.RegisterModuleScopedFunction(
          func_id, {.rel_byte_offsets = &input.layout.rel_byte_offsets});
    }
  }

  // Step 3: Declare all body functions
  std::vector<std::pair<mir::FunctionId, llvm::Function*>> declared_funcs;
  std::unordered_set<uint32_t> seen_func_ids;
  for (mir::FunctionId func_id : input.functions) {
    if (!seen_func_ids.insert(func_id.value).second) continue;
    auto llvm_func_or_err = DeclareUserFunction(
        context, func_id,
        std::format("body_{}_func_{}", input.body_id.value, func_id.value));
    if (!llvm_func_or_err) return std::unexpected(llvm_func_or_err.error());
    declared_funcs.emplace_back(func_id, *llvm_func_or_err);
  }

  // Step 4: Define all body functions
  for (const auto& [func_id, llvm_func] : declared_funcs) {
    auto result = DefineUserFunction(context, func_id, llvm_func);
    if (!result) return std::unexpected(result.error());
  }

  // Step 5: Codegen all body-owned processes
  CompiledModuleSpec product{
      .body_id = input.body_id,
      .process_functions = {},
      .wait_sites = {},
  };

  for (const auto& proc_view : input.view.processes) {
    context.SetCurrentProcess(proc_view.layout_process_index);
    // Compatibility: process lowering still needs a representative instance ID
    // for %m path support. This is transitional -- true specialization codegen
    // should not require instance identity.
    context.SetCurrentInstanceId(proc_view.representative_module_index.value);
    context.SetRelByteOffsets(&input.layout.rel_byte_offsets);

    const auto& mir_process = arena[proc_view.process_id];
    auto func_result = GenerateSharedProcessFunction(
        context, mir_process, proc_view.func_name);
    if (!func_result) return std::unexpected(func_result.error());

    product.process_functions.push_back(func_result->function);

    product.wait_sites.insert(
        product.wait_sites.end(),
        std::make_move_iterator(func_result->wait_sites.begin()),
        std::make_move_iterator(func_result->wait_sites.end()));
  }

  return product;
}

auto CompileDesignProcesses(const LoweringInput& input)
    -> Result<CodegenSession> {
  // Phase 0: Backend/session setup
  auto llvm_ctx = std::make_unique<llvm::LLVMContext>();
  auto module = std::make_unique<llvm::Module>("lyra_module", *llvm_ctx);

  SetHostDataLayout(*module);

  bool force_two_state = input.force_two_state;

  auto slot_info = BuildSlotInfoFromDesign(
      *input.design, *input.type_arena, force_two_state);

  auto design_layout = BuildDesignLayout(
      slot_info, *input.type_arena, *llvm_ctx, module->getDataLayout(),
      force_two_state);

  auto layout = std::make_unique<Layout>(BuildLayout(
      *input.design, *input.mir_arena, *input.type_arena,
      std::move(design_layout), *llvm_ctx, module->getDataLayout(),
      force_two_state));

  auto context = std::make_unique<Context>(
      *input.mir_arena, *input.type_arena, *layout, std::move(llvm_ctx),
      std::move(module), input.diag_ctx, force_two_state);

  bool loop_guard_enabled = runtime::HasFlag(
      static_cast<runtime::FeatureFlag>(input.feature_flags),
      runtime::FeatureFlag::kEnableLoopGuard);
  context->SetLoopGuardEnabled(loop_guard_enabled);

  // Phase 1: Build specialization inputs (units + layouts + codegen views)
  auto units = BuildSpecCompilationUnits(*input.design);
  auto modidx_to_sched_indices = BuildModuleSchedIndices(*layout);
  auto views =
      BuildSpecCodegenViews(units, *input.mir_arena, modidx_to_sched_indices);
  auto spec_layouts = BuildSpecLayouts(units, *layout);
  auto spec_inputs = BuildCompiledModuleSpecInputs(
      units, std::move(spec_layouts), std::move(views));

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
    auto llvm_func_or_err = DeclareUserFunction(
        *context, func_id, std::format("global_func_{}", func_id.value));
    if (!llvm_func_or_err) return std::unexpected(llvm_func_or_err.error());
    declared_funcs.emplace_back(func_id, *llvm_func_or_err);
  }

  for (const auto& [func_id, llvm_func] : declared_funcs) {
    auto result = DefineUserFunction(*context, func_id, llvm_func);
    if (!result) return std::unexpected(result.error());
  }

  // Phase 4: Compile each specialization via CompileModuleSpecSession
  // Build body_id -> compiled process functions routing table
  std::unordered_map<uint32_t, std::vector<llvm::Function*>>
      body_to_compiled_funcs;
  std::vector<WaitSiteEntry> all_wait_sites;

  for (size_t si = 0; si < spec_inputs.size(); ++si) {
    const auto& spec_input = spec_inputs[si];
    auto product =
        CompileModuleSpecSession(*context, *input.mir_arena, spec_input);
    if (!product) return std::unexpected(product.error());

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

  // Build explicit sched_index -> shared compiled function routing table.
  // This maps each scheduled module process to its body-owned compiled
  // function, using the pre-computed per-instance scheduled-process indices.
  std::unordered_map<uint32_t, llvm::Function*> sched_idx_to_shared_func;
  for (const auto& unit : units) {
    auto funcs_it = body_to_compiled_funcs.find(unit.body_id.value);
    if (funcs_it == body_to_compiled_funcs.end()) {
      throw common::InternalError(
          "CompileDesignProcesses",
          std::format("no compiled functions for body {}", unit.body_id.value));
    }
    const auto& compiled_funcs = funcs_it->second;

    for (const auto& inst : unit.instances) {
      auto sched_it = modidx_to_sched_indices.find(inst.module_index.value);
      if (sched_it == modidx_to_sched_indices.end()) {
        if (!compiled_funcs.empty()) {
          throw common::InternalError(
              "CompileDesignProcesses",
              std::format(
                  "body {} has {} compiled functions but instance {} has "
                  "no scheduled processes",
                  unit.body_id.value, compiled_funcs.size(),
                  inst.module_index.value));
        }
        continue;
      }
      const auto& sched_indices = sched_it->second;
      if (sched_indices.size() != compiled_funcs.size()) {
        throw common::InternalError(
            "CompileDesignProcesses",
            std::format(
                "body {} has {} compiled functions but instance {} has {} "
                "scheduled processes",
                unit.body_id.value, compiled_funcs.size(),
                inst.module_index.value, sched_indices.size()));
      }
      for (size_t k = 0; k < sched_indices.size(); ++k) {
        auto [it, inserted] = sched_idx_to_shared_func.try_emplace(
            sched_indices[k], compiled_funcs[k]);
        if (!inserted) {
          throw common::InternalError(
              "CompileDesignProcesses",
              std::format(
                  "duplicate routing for scheduled process index {}",
                  sched_indices[k]));
        }
      }
    }
  }

  // Phase 5: Per-instance wrappers / standalone entrypoints
  std::vector<llvm::Function*> process_funcs;
  process_funcs.reserve(layout->scheduled_processes.size());

  size_t num_init = layout->num_init_processes;
  for (size_t i = 0; i < layout->scheduled_processes.size(); ++i) {
    context->SetCurrentProcess(i);

    const auto& bp = layout->scheduled_processes[i];
    const auto& mir_process = (*input.mir_arena)[bp.process_id];
    context->SetCurrentInstanceId(bp.module_index.value);

    // Init and connection processes are standalone (no module binding)
    if (i < num_init || bp.module_index.value == ModuleIndex::kNone) {
      auto func_result = GenerateProcessFunction(
          *context, mir_process, std::format("process_{}", i));
      if (!func_result) return std::unexpected(func_result.error());
      process_funcs.push_back(func_result->function);
      all_wait_sites.insert(
          all_wait_sites.end(),
          std::make_move_iterator(func_result->wait_sites.begin()),
          std::make_move_iterator(func_result->wait_sites.end()));
      continue;
    }

    // Module process: route through explicit sched_idx -> shared_func table
    auto func_it = sched_idx_to_shared_func.find(static_cast<uint32_t>(i));
    if (func_it == sched_idx_to_shared_func.end()) {
      throw common::InternalError(
          "CompileDesignProcesses",
          std::format(
              "no compiled function routing for scheduled process {}", i));
    }
    llvm::Function* shared_func = func_it->second;

    uint64_t base_byte_offset =
        layout->GetInstanceBaseByteOffset(bp.module_index);
    uint32_t base_slot_id = mir::GetInstanceBaseSlot(
        input.design->placement, bp.module_index.value);

    auto* wrapper = GenerateProcessWrapper(
        *context, shared_func, bp.module_index.value, base_byte_offset,
        base_slot_id, std::format("process_{}", i));
    process_funcs.push_back(wrapper);
  }

  return CodegenSession{
      .layout = std::move(layout),
      .context = std::move(context),
      .design = input.design,
      .process_funcs = std::move(process_funcs),
      .wait_sites = std::move(all_wait_sites),
      .slot_info = std::move(slot_info),
      .num_init_processes = num_init,
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
  auto* design_type = context.GetDesignStateType();

  for (const auto& var : variables) {
    if (var.slot_id >= slots.size()) {
      continue;
    }

    auto slot_id = mir::SlotId{static_cast<uint32_t>(var.slot_id)};
    uint32_t field_index = context.GetDesignFieldIndex(slot_id);
    auto* slot_ptr = builder.CreateStructGEP(
        design_type, design_state, field_index, "var_ptr");

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
