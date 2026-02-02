#include "lyra/llvm_backend/lower.hpp"

#include <cstddef>
#include <cstdint>
#include <expected>
#include <format>
#include <memory>
#include <mutex>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include <llvm/ADT/StringMap.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Support/Casting.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/TargetParser/Host.h>
#include <llvm/TargetParser/SubtargetFeature.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_utils.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/layout/layout.hpp"
#include "lyra/llvm_backend/process.hpp"
#include "lyra/llvm_backend/type_ops/default_init.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/module.hpp"
#include "lyra/mir/statement.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

// Initialize LLVM native targets (thread-safe, once per process).
void InitializeLlvmTargets() {
  static std::once_flag flag;
  std::call_once(flag, [] {
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();
  });
}

// Set module's DataLayout and TargetTriple to match the host.
// This MUST be called before any IR generation that uses DataLayout
// (e.g., getTypeAllocSize for memset).
//
// Uses host CPU and features to ensure DataLayout matches what ORC JIT expects.
void SetHostDataLayout(llvm::Module& module) {
  InitializeLlvmTargets();

  std::string triple = llvm::sys::getDefaultTargetTriple();
  std::string cpu = llvm::sys::getHostCPUName().str();

  // Collect host CPU features
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

// Initialize DesignState: zero everything, then overwrite 4-state slots with X
void InitializeDesignState(
    Context& context, llvm::Value* design_state,
    const std::vector<SlotInfo>& slots) {
  auto& builder = context.GetBuilder();
  auto* design_type = context.GetDesignStateType();
  const auto& types = context.GetTypeArena();

  // Zero-initialize via memset (avoids LLVM assertion on wide aggregates)
  EmitMemsetZero(context, design_state, design_type);

  // Overwrite slots containing 4-state fields with proper X encoding
  for (const auto& slot : slots) {
    if (!IsFourStateType(slot.type_id, types)) {
      continue;  // Zero-init is correct for pure 2-state types
    }
    uint32_t field_index = context.GetDesignFieldIndex(slot.slot_id);
    auto* slot_ptr =
        builder.CreateStructGEP(design_type, design_state, field_index);
    EmitSVDefaultInit(context, slot_ptr, slot.type_id);
  }
}

// Initialize ProcessStateN: zero everything, set design pointer, then
// overwrite 4-state frame places with X encoding.
// Takes ProcessLayout explicitly to avoid implicit dependency on
// Context::SetCurrentProcess.
void InitializeProcessState(
    Context& context, llvm::Value* process_state, llvm::Value* design_state,
    const ProcessLayout& proc_layout) {
  auto& builder = context.GetBuilder();
  auto* state_type = proc_layout.state_type;
  auto* header_type = context.GetHeaderType();
  const auto& types = context.GetTypeArena();

  // Zero-initialize via memset (avoids LLVM assertion on wide aggregates)
  EmitMemsetZero(context, process_state, state_type);

  // Set design pointer: header->design (field 1)
  auto* header_ptr = builder.CreateStructGEP(state_type, process_state, 0);
  auto* design_ptr_ptr = builder.CreateStructGEP(header_type, header_ptr, 1);
  builder.CreateStore(design_state, design_ptr_ptr);

  // Overwrite slots containing 4-state fields with proper X encoding
  auto* frame_type = proc_layout.frame.llvm_type;
  auto* frame_ptr = builder.CreateStructGEP(state_type, process_state, 1);
  const auto& frame_layout = proc_layout.frame;

  for (uint32_t i = 0; i < frame_layout.root_types.size(); ++i) {
    TypeId type_id = frame_layout.root_types[i];
    if (!IsFourStateType(type_id, types)) {
      continue;  // Zero-init is correct for pure 2-state types
    }
    auto* field_ptr = builder.CreateStructGEP(frame_type, frame_ptr, i);
    EmitSVDefaultInit(context, field_ptr, type_id);
  }
}

// Release all string slots to prevent memory leaks
void ReleaseStringSlots(
    Context& context, const std::vector<SlotInfo>& slots,
    llvm::Value* design_state) {
  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  auto* ptr_ty = llvm::PointerType::getUnqual(llvm_ctx);
  auto* design_type = context.GetDesignStateType();

  for (const auto& slot : slots) {
    if (slot.type_info.kind != VarTypeKind::kString) {
      continue;
    }

    uint32_t field_index = context.GetDesignFieldIndex(slot.slot_id);
    auto* slot_ptr =
        builder.CreateStructGEP(design_type, design_state, field_index);
    auto* val = builder.CreateLoad(ptr_ty, slot_ptr);
    builder.CreateCall(context.GetLyraStringRelease(), {val});
  }
}

}  // namespace

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

auto LowerMirToLlvm(const LoweringInput& input) -> Result<LoweringResult> {
  // Create LLVM context and module
  auto llvm_ctx = std::make_unique<llvm::LLVMContext>();
  auto module = std::make_unique<llvm::Module>("lyra_module", *llvm_ctx);

  // Set DataLayout BEFORE any IR generation that depends on type
  // sizes/alignment. Without this, getTypeAllocSize() uses default rules that
  // may differ from the target's actual layout, causing size mismatches (e.g.,
  // memset length).
  SetHostDataLayout(*module);

  // Build slot info from design's slot_table (MIR is single source of truth)
  auto slot_info = BuildSlotInfoFromDesign(*input.design, *input.type_arena);

  // Build layout first (pure analysis, no LLVM IR emission)
  Layout layout = BuildLayout(
      *input.design, *input.mir_arena, *input.type_arena, slot_info, *llvm_ctx);

  // Create context with layout
  Context context(
      *input.design, *input.mir_arena, *input.type_arena, layout,
      std::move(llvm_ctx), std::move(module), input.diag_ctx);

  auto& builder = context.GetBuilder();
  auto& ctx = context.GetLlvmContext();
  auto& mod = context.GetModule();

  // Pre-scan all processes for MonitorEffect instructions and register their
  // thunk info in the context. This must happen before DeclareUserFunction
  // because:
  // - Check thunks have a different signature (3 args instead of 2)
  // - Setup thunks need appended serialization + registration code
  auto register_monitor_info = [&](mir::ProcessId proc_id) {
    const auto& process = (*input.mir_arena)[proc_id];
    for (const auto& block : process.blocks) {
      for (const auto& instr : block.statements) {
        const auto* effect = std::get_if<mir::Effect>(&instr.data);
        if (effect == nullptr) continue;
        const auto* monitor = std::get_if<mir::MonitorEffect>(&effect->op);
        if (monitor == nullptr) continue;

        // Register monitor layout (offsets/sizes only - format_ops come from
        // check thunk's own DisplayEffect to ensure correct MIR operand
        // context)
        Context::MonitorLayout layout{
            .offsets = monitor->offsets,
            .byte_sizes = monitor->byte_sizes,
            .total_size = monitor->prev_buffer_size,
        };
        context.RegisterMonitorLayout(monitor->check_thunk, std::move(layout));

        // Register setup thunk marker (just references check_thunk for layout)
        Context::MonitorSetupInfo setup_info{
            .check_thunk = monitor->check_thunk,
        };
        context.RegisterMonitorSetupInfo(
            monitor->setup_thunk, std::move(setup_info));
      }
    }
  };
  // Scan module processes
  for (const auto& element : input.design->elements) {
    if (const auto* mod_elem = std::get_if<mir::Module>(&element)) {
      for (mir::ProcessId proc_id : mod_elem->processes) {
        register_monitor_info(proc_id);
      }
    }
  }
  // Scan init_processes (package variable initialization)
  for (mir::ProcessId proc_id : input.design->init_processes) {
    register_monitor_info(proc_id);
  }

  // Two-pass user function generation for mutual recursion support
  // Collect all function IDs from modules, packages, and generated functions
  std::vector<mir::FunctionId> all_func_ids;
  for (const auto& element : input.design->elements) {
    std::visit(
        [&](const auto& elem) {
          for (mir::FunctionId func_id : elem.functions) {
            all_func_ids.push_back(func_id);
          }
        },
        element);
  }
  // Include dynamically generated functions (e.g., strobe thunks)
  for (mir::FunctionId func_id : input.design->generated_functions) {
    all_func_ids.push_back(func_id);
  }

  // Pass 1: Declare all user functions (builds function types, registers them)
  std::vector<std::pair<mir::FunctionId, llvm::Function*>> declared_funcs;
  declared_funcs.reserve(all_func_ids.size());
  for (size_t i = 0; i < all_func_ids.size(); ++i) {
    mir::FunctionId func_id = all_func_ids[i];
    auto llvm_func_or_err =
        DeclareUserFunction(context, func_id, std::format("user_func_{}", i));
    if (!llvm_func_or_err) return std::unexpected(llvm_func_or_err.error());
    llvm::Function* llvm_func = *llvm_func_or_err;
    declared_funcs.emplace_back(func_id, llvm_func);
  }

  // Pass 2: Define all user functions (emits bodies, can reference other funcs)
  for (const auto& [func_id, llvm_func] : declared_funcs) {
    auto result = DefineUserFunction(context, func_id, llvm_func);
    if (!result) return std::unexpected(result.error());
  }

  // Generate process functions (use process_ids from layout for single source
  // of truth)
  std::vector<llvm::Function*> process_funcs;
  process_funcs.reserve(layout.process_ids.size());

  for (size_t i = 0; i < layout.process_ids.size(); ++i) {
    context.SetCurrentProcess(i);

    // Use the canonical process list from layout
    mir::ProcessId proc_id = layout.process_ids[i];
    const auto& mir_process = (*input.mir_arena)[proc_id];

    // Set instance_id for %m support
    context.SetCurrentInstanceId(mir_process.owner_instance_id);

    auto func_result = GenerateProcessFunction(
        context, mir_process, std::format("process_{}", i));
    if (!func_result) return std::unexpected(func_result.error());
    process_funcs.push_back(*func_result);
  }

  // Create main function: int main()
  // Note: "main" is a compiler-owned symbol synthesized by Lyra as the
  // simulation entry point. User code cannot define main() - it is reserved.
  // This is the contract between the compiler and the jit/lli backends.
  // If user-defined main() becomes possible later, rename to "lyra_entry".
  auto* main_type = llvm::FunctionType::get(llvm::Type::getInt32Ty(ctx), false);
  auto* main_func = llvm::Function::Create(
      main_type, llvm::Function::ExternalLinkage, "main", &mod);

  auto* entry = llvm::BasicBlock::Create(ctx, "entry", main_func);
  auto* exit_block = llvm::BasicBlock::Create(ctx, "exit", main_func);
  builder.SetInsertPoint(entry);

  // Allocate DesignState (shared by all processes)
  auto* design_state = builder.CreateAlloca(
      context.GetDesignStateType(), nullptr, "design_state");

  // Initialize DesignState (zero + 4-state X init)
  InitializeDesignState(context, design_state, slot_info);

  // Initialize runtime state (reset time tracker, set fs_base_dir)
  auto* fs_base_dir_str =
      builder.CreateGlobalStringPtr(input.fs_base_dir, "fs_base_dir");
  builder.CreateCall(context.GetLyraInitRuntime(), {fs_base_dir_str});

  // Hook: after design state and runtime are initialized
  if (input.hooks != nullptr) {
    input.hooks->OnAfterInitializeDesignState(context, slot_info, design_state);
  }

  auto* ptr_ty = llvm::PointerType::getUnqual(ctx);
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);

  // Phase 1: Init processes (package variable initialization)
  // Run synchronously before scheduler, matching MIR interpreter behavior
  size_t num_init = layout.num_init_processes;
  for (size_t i = 0; i < num_init; ++i) {
    const auto& proc_layout = layout.processes[i];

    auto* process_state = builder.CreateAlloca(
        proc_layout.state_type, nullptr, std::format("init_state_{}", i));

    InitializeProcessState(context, process_state, design_state, proc_layout);

    // Run synchronously to completion via runtime API
    // (entry block number is encapsulated in LyraRunProcessSync)
    builder.CreateCall(
        context.GetLyraRunProcessSync(), {process_funcs[i], process_state});
  }

  // Hook: after init processes, before module simulation
  if (input.hooks != nullptr) {
    input.hooks->OnBeforeRunSimulation(context, slot_info, design_state);
  }

  // Phase 2: Module processes through scheduler
  auto num_module_processes =
      static_cast<uint32_t>(process_funcs.size() - num_init);

  if (num_module_processes > 0) {
    std::vector<llvm::Value*> module_states;
    module_states.reserve(num_module_processes);

    for (size_t i = num_init; i < process_funcs.size(); ++i) {
      const auto& proc_layout = layout.processes[i];

      auto* process_state = builder.CreateAlloca(
          proc_layout.state_type, nullptr, std::format("process_state_{}", i));

      InitializeProcessState(context, process_state, design_state, proc_layout);
      module_states.push_back(process_state);
    }

    // Build function pointer and state pointer arrays for scheduler
    auto* funcs_array = builder.CreateAlloca(
        ptr_ty, llvm::ConstantInt::get(i32_ty, num_module_processes),
        "funcs_array");
    auto* states_array = builder.CreateAlloca(
        ptr_ty, llvm::ConstantInt::get(i32_ty, num_module_processes),
        "states_array");

    for (uint32_t i = 0; i < num_module_processes; ++i) {
      auto* func_slot =
          builder.CreateGEP(ptr_ty, funcs_array, {builder.getInt32(i)});
      builder.CreateStore(process_funcs[num_init + i], func_slot);

      auto* state_slot =
          builder.CreateGEP(ptr_ty, states_array, {builder.getInt32(i)});
      builder.CreateStore(module_states[i], state_slot);
    }

    // Build plusargs array for $test$plusargs and $value$plusargs
    auto num_plusargs = static_cast<uint32_t>(input.plusargs.size());
    llvm::Value* plusargs_array = nullptr;
    if (num_plusargs > 0) {
      plusargs_array = builder.CreateAlloca(
          ptr_ty, llvm::ConstantInt::get(i32_ty, num_plusargs), "plusargs");
      for (uint32_t i = 0; i < num_plusargs; ++i) {
        auto* plusarg_str = builder.CreateGlobalStringPtr(
            input.plusargs[i], std::format("plusarg_{}", i));
        auto* slot =
            builder.CreateGEP(ptr_ty, plusargs_array, {builder.getInt32(i)});
        builder.CreateStore(plusarg_str, slot);
      }
    } else {
      plusargs_array =
          llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptr_ty));
    }

    // Build instance paths array for %m support
    const auto& instance_entries = input.design->instance_table.entries;
    auto num_instance_paths = static_cast<uint32_t>(instance_entries.size());
    llvm::Value* instance_paths_array = nullptr;
    if (num_instance_paths > 0) {
      instance_paths_array = builder.CreateAlloca(
          ptr_ty, llvm::ConstantInt::get(i32_ty, num_instance_paths),
          "instance_paths");
      for (uint32_t i = 0; i < num_instance_paths; ++i) {
        auto* path_str = builder.CreateGlobalStringPtr(
            instance_entries[i].full_path, std::format("inst_path_{}", i));
        auto* slot = builder.CreateGEP(
            ptr_ty, instance_paths_array, {builder.getInt32(i)});
        builder.CreateStore(path_str, slot);
      }
    } else {
      instance_paths_array =
          llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptr_ty));
    }

    // Call multi-process scheduler
    builder.CreateCall(
        context.GetLyraRunSimulation(),
        {funcs_array, states_array,
         llvm::ConstantInt::get(i32_ty, num_module_processes), plusargs_array,
         llvm::ConstantInt::get(i32_ty, num_plusargs), instance_paths_array,
         llvm::ConstantInt::get(i32_ty, num_instance_paths)});
  }

  // Branch to exit block
  builder.CreateBr(exit_block);

  // Exit block: release strings, run hooks (if any), return 0
  builder.SetInsertPoint(exit_block);

  // Release all string locals to prevent memory leaks
  ReleaseStringSlots(context, slot_info, design_state);

  // Hook: after simulation completes
  if (input.hooks != nullptr) {
    input.hooks->OnAfterRunSimulation(context, slot_info, design_state);
  }

  builder.CreateRet(llvm::ConstantInt::get(ctx, llvm::APInt(32, 0)));

  auto [result_ctx, result_mod] = context.TakeOwnership();
  return LoweringResult{
      .context = std::move(result_ctx),
      .module = std::move(result_mod),
  };
}

auto DumpLlvmIr(const LoweringResult& result) -> std::string {
  std::string ir;
  llvm::raw_string_ostream stream(ir);
  result.module->print(stream, nullptr);
  return ir;
}

}  // namespace lyra::lowering::mir_to_llvm
