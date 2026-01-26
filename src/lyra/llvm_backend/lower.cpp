#include "lyra/llvm_backend/lower.hpp"

#include <cstddef>
#include <cstdint>
#include <expected>
#include <format>
#include <memory>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/Casting.h>
#include <llvm/Support/raw_ostream.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/layout.hpp"
#include "lyra/llvm_backend/process.hpp"
#include "lyra/mir/handle.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

// Store X-encoded value ({value=0, unknown=semantic_mask}) to a 4-state pointer
void StoreFourStateX(
    llvm::IRBuilder<>& builder, llvm::Value* ptr, llvm::StructType* struct_type,
    uint32_t semantic_width) {
  auto* elem_type = struct_type->getElementType(0);
  auto* zero = llvm::ConstantInt::get(elem_type, 0);
  uint32_t storage_width = elem_type->getIntegerBitWidth();
  auto unk_mask = llvm::APInt::getLowBitsSet(storage_width, semantic_width);
  auto* unk_val = llvm::ConstantInt::get(elem_type, unk_mask);

  // Build {value=0, unknown=semantic_mask}
  llvm::Value* init = llvm::UndefValue::get(struct_type);
  init = builder.CreateInsertValue(init, zero, 0);
  init = builder.CreateInsertValue(init, unk_val, 1);
  builder.CreateStore(init, ptr);
}

// Initialize DesignState: zero everything, then overwrite 4-state slots with X
void InitializeDesignState(
    Context& context, llvm::Value* design_state,
    const std::vector<SlotInfo>& slots) {
  auto& builder = context.GetBuilder();
  auto* design_type = context.GetDesignStateType();
  const auto& types = context.GetTypeArena();

  // Use aggregate zero for simple initialization (covers 2-state fields)
  auto* zero = llvm::ConstantAggregateZero::get(design_type);
  builder.CreateStore(zero, design_state);

  // Overwrite 4-state slots with X encoding (scalar packed types only)
  for (const auto& slot : slots) {
    const Type& type = types[slot.type_id];
    if (!IsPacked(type) || !IsPackedFourState(type, types)) {
      continue;
    }
    uint32_t field_index = context.GetDesignFieldIndex(slot.slot_id);
    auto* field_type = design_type->getElementType(field_index);
    auto* struct_type = llvm::cast<llvm::StructType>(field_type);
    uint32_t semantic_width = PackedBitWidth(type, types);
    auto* slot_ptr =
        builder.CreateStructGEP(design_type, design_state, field_index);
    StoreFourStateX(builder, slot_ptr, struct_type, semantic_width);
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

  // Use aggregate zero for the entire state
  auto* zero = llvm::ConstantAggregateZero::get(state_type);
  builder.CreateStore(zero, process_state);

  // Set design pointer: header->design (field 1)
  auto* header_ptr = builder.CreateStructGEP(state_type, process_state, 0);
  auto* design_ptr_ptr = builder.CreateStructGEP(header_type, header_ptr, 1);
  builder.CreateStore(design_state, design_ptr_ptr);

  // Overwrite 4-state frame places with X encoding
  auto* frame_type = proc_layout.frame.llvm_type;
  auto* frame_ptr = builder.CreateStructGEP(state_type, process_state, 1);
  const auto& frame_layout = proc_layout.frame;

  for (uint32_t i = 0; i < frame_layout.root_types.size(); ++i) {
    TypeId type_id = frame_layout.root_types[i];
    const Type& type = types[type_id];
    // Only scalar packed types get X initialization; aggregates use zero
    if (!IsPacked(type) || !IsPackedFourState(type, types)) {
      continue;
    }
    auto* field_type = frame_type->getElementType(i);
    auto* struct_type = llvm::cast<llvm::StructType>(field_type);
    uint32_t semantic_width = PackedBitWidth(type, types);
    auto* field_ptr = builder.CreateStructGEP(frame_type, frame_ptr, i);
    StoreFourStateX(builder, field_ptr, struct_type, semantic_width);
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
    builder.CreateCall(
        context.GetLyraRegisterVar(),
        {name_ptr, slot_ptr, kind_val, width_val, signed_val});
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

    auto func_result = GenerateProcessFunction(
        context, mir_process, std::format("process_{}", i));
    if (!func_result) return std::unexpected(func_result.error());
    process_funcs.push_back(*func_result);
  }

  // Create main function: int main()
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

    // Call multi-process scheduler
    builder.CreateCall(
        context.GetLyraRunSimulation(),
        {funcs_array, states_array,
         llvm::ConstantInt::get(i32_ty, num_module_processes)});
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
