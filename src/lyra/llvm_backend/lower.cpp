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
#include <utility>
#include <variant>
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

auto BuildSpecCompilationUnits(const mir::Design& design, const Layout& layout)
    -> std::vector<SpecCompilationUnit> {
  std::vector<SpecCompilationUnit> units;
  std::unordered_map<uint32_t, size_t> body_to_unit;
  // Maps ModuleIndex -> body_id for template association.
  std::unordered_map<uint32_t, uint32_t> modidx_to_bodyid;

  uint32_t module_idx = 0;
  for (const auto& element : design.elements) {
    const auto* mod = std::get_if<mir::Module>(&element);
    if (mod == nullptr) continue;

    auto body_id = mod->body_id;
    modidx_to_bodyid[module_idx] = body_id.value;

    const auto& placement =
        mir::GetInstancePlacement(design.placement, module_idx);

    SpecInstanceBinding binding{
        .module_index = ModuleIndex{module_idx},
        .base_slot_id = placement.design_state_base_slot,
    };

    auto [it, inserted] = body_to_unit.try_emplace(body_id.value, units.size());
    if (inserted) {
      units.push_back(
          SpecCompilationUnit{
              .body_id = body_id,
              .instances = {binding},
              .template_indices = {},
          });
    } else {
      units[it->second].instances.push_back(binding);
    }

    ++module_idx;
  }

  // Associate template indices with their owning body's unit.
  for (size_t t = 0; t < layout.process_templates.size(); ++t) {
    const auto& tmpl = layout.process_templates[t];
    auto body_it = modidx_to_bodyid.find(tmpl.representative_module_idx.value);
    if (body_it == modidx_to_bodyid.end()) continue;
    auto unit_it = body_to_unit.find(body_it->second);
    if (unit_it == body_to_unit.end()) continue;
    units[unit_it->second].template_indices.push_back(t);
  }

  return units;
}

}  // namespace

auto PrepareSpecialization(
    Context& context, const Layout& layout, const mir::Arena& arena,
    const mir::Design& design, const SpecCompilationUnit& unit)
    -> PreparedSpecialization {
  const auto& body = design.module_bodies.at(unit.body_id.value);

  // Register monitor info for body processes
  for (mir::ProcessId proc_id : body.processes) {
    RegisterMonitorInfo(context, arena, proc_id);
  }

  // Register module-scoped function metadata for each instance
  for (const auto& binding : unit.instances) {
    const auto& variant = layout.GetInstanceVariant(binding.module_index);
    for (mir::FunctionId func_id : body.functions) {
      const auto& func = arena[func_id];
      if (func.thunk_kind == mir::ThunkKind::kNone) {
        context.RegisterModuleScopedFunction(
            func_id, {.rel_byte_offsets = &variant.rel_byte_offsets,
                      .base_slot_id = binding.base_slot_id});
      }
    }
  }

  // Collect body function IDs
  PreparedSpecialization prepared{
      .body_id = unit.body_id,
      .function_ids = {body.functions.begin(), body.functions.end()},
  };

  return prepared;
}

auto CompileDesignProcesses(const LoweringInput& input)
    -> Result<CodegenSession> {
  // Phase 0: Setup
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

  // Phase 1: Build specialization units
  auto units = BuildSpecCompilationUnits(*input.design, *layout);

  // Phase 2: Design-wide init-process monitor registration
  for (mir::ProcessId proc_id : input.design->init_processes) {
    RegisterMonitorInfo(*context, *input.mir_arena, proc_id);
  }

  // Phase 3: Specialization-owned preparation (monitors, function metadata,
  // function collection)
  std::vector<PreparedSpecialization> prepared_specs;
  prepared_specs.reserve(units.size());
  for (const auto& unit : units) {
    prepared_specs.push_back(PrepareSpecialization(
        *context, *layout, *input.mir_arena, *input.design, unit));
  }

  // Merge specialization function IDs into global collection
  std::vector<mir::FunctionId> all_func_ids;
  std::unordered_set<uint32_t> seen_func_ids;
  for (const auto& prepared : prepared_specs) {
    for (mir::FunctionId func_id : prepared.function_ids) {
      if (seen_func_ids.insert(func_id.value).second) {
        all_func_ids.push_back(func_id);
      }
    }
  }

  // Phase 4: Design-wide function collection outside specialization units
  for (const auto& element : input.design->elements) {
    const auto* pkg = std::get_if<mir::Package>(&element);
    if (pkg == nullptr) continue;
    for (mir::FunctionId func_id : pkg->functions) {
      if (seen_func_ids.insert(func_id.value).second) {
        all_func_ids.push_back(func_id);
      }
    }
  }
  for (mir::FunctionId func_id : input.design->generated_functions) {
    if (seen_func_ids.insert(func_id.value).second) {
      all_func_ids.push_back(func_id);
    }
  }

  // Phase 5: Global function two-pass (declare all, then define all)
  std::vector<std::pair<mir::FunctionId, llvm::Function*>> declared_funcs;
  declared_funcs.reserve(all_func_ids.size());
  for (size_t i = 0; i < all_func_ids.size(); ++i) {
    mir::FunctionId func_id = all_func_ids[i];
    auto llvm_func_or_err =
        DeclareUserFunction(*context, func_id, std::format("user_func_{}", i));
    if (!llvm_func_or_err) return std::unexpected(llvm_func_or_err.error());
    llvm::Function* llvm_func = *llvm_func_or_err;
    declared_funcs.emplace_back(func_id, llvm_func);
  }

  for (const auto& [func_id, llvm_func] : declared_funcs) {
    auto result = DefineUserFunction(*context, func_id, llvm_func);
    if (!result) return std::unexpected(result.error());
  }

  // Phase 6: Specialization template process generation (step 4).
  // Must happen after function declare/define since template codegen
  // may reference user functions.
  std::vector<llvm::Function*> template_fns(layout->process_templates.size());
  std::vector<WaitSiteEntry> all_wait_sites;

  for (const auto& unit : units) {
    auto result = CompileSpecialization(
        *context, *layout, *input.mir_arena, unit, template_fns,
        all_wait_sites);
    if (!result) return std::unexpected(result.error());
  }

  // Phase 7: Per-instance wrapper / standalone process generation
  std::vector<llvm::Function*> process_funcs;
  process_funcs.reserve(layout->scheduled_processes.size());

  size_t num_init = layout->num_init_processes;
  for (size_t i = 0; i < layout->scheduled_processes.size(); ++i) {
    context->SetCurrentProcess(i);

    const auto& bp = layout->scheduled_processes[i];
    const auto& mir_process = (*input.mir_arena)[bp.process_id];
    context->SetCurrentInstanceId(bp.module_index.value);

    auto route =
        layout->RouteProcess(LayoutProcessIndex{static_cast<uint32_t>(i)});
    if (auto* templated = std::get_if<TemplatedRoute>(&route)) {
      uint64_t base_byte_offset =
          layout->GetInstanceBaseByteOffset(templated->module_idx);
      uint32_t base_slot_id = mir::GetInstanceBaseSlot(
          input.design->placement, templated->module_idx.value);

      auto* wrapper = GenerateProcessWrapper(
          *context, template_fns[templated->template_id.value],
          bp.module_index.value, base_byte_offset, base_slot_id,
          std::format("process_{}", i));
      process_funcs.push_back(wrapper);
      continue;
    }

    auto func_result = GenerateProcessFunction(
        *context, mir_process, std::format("process_{}", i));
    if (!func_result) return std::unexpected(func_result.error());
    process_funcs.push_back(func_result->function);
    all_wait_sites.insert(
        all_wait_sites.end(),
        std::make_move_iterator(func_result->wait_sites.begin()),
        std::make_move_iterator(func_result->wait_sites.end()));
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

auto CompileSpecialization(
    Context& context, const Layout& layout, const mir::Arena& arena,
    const SpecCompilationUnit& unit, std::vector<llvm::Function*>& template_fns,
    std::vector<WaitSiteEntry>& all_wait_sites) -> Result<void> {
  // Generate shared/template process functions for this body
  for (size_t t : unit.template_indices) {
    const auto& tmpl = layout.process_templates[t];
    context.SetCurrentProcess(tmpl.template_layout_index.value);

    const auto& mir_process = arena[tmpl.template_process];
    context.SetCurrentInstanceId(tmpl.representative_module_idx.value);

    const auto& variant =
        layout.GetInstanceVariant(tmpl.representative_module_idx);
    context.SetRelByteOffsets(
        variant.rel_byte_offsets, tmpl.template_base_slot_id);

    auto func_result =
        GenerateSharedProcessFunction(context, mir_process, tmpl.func_name);
    if (!func_result) return std::unexpected(func_result.error());
    template_fns[t] = func_result->function;

    all_wait_sites.insert(
        all_wait_sites.end(),
        std::make_move_iterator(func_result->wait_sites.begin()),
        std::make_move_iterator(func_result->wait_sites.end()));
  }

  return {};
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
