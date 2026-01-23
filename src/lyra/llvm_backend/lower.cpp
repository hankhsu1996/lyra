#include "lyra/llvm_backend/lower.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <memory>
#include <string>
#include <utility>
#include <vector>

#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/raw_ostream.h"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/common/type_utils.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/layout.hpp"
#include "lyra/llvm_backend/process.hpp"
#include "lyra/mir/handle.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

// Build SlotInfo list from LoweringInput
auto BuildSlotInfoList(
    const std::vector<SlotTypeInfo>& slot_types,
    const std::vector<TypeId>& type_ids) -> std::vector<SlotInfo> {
  if (slot_types.size() != type_ids.size()) {
    throw common::InternalError(
        "BuildSlotInfoList",
        std::format(
            "slot_types.size() ({}) != slot_type_ids.size() ({})",
            slot_types.size(), type_ids.size()));
  }

  std::vector<SlotInfo> slots;
  slots.reserve(slot_types.size());

  for (size_t i = 0; i < slot_types.size(); ++i) {
    slots.push_back(
        SlotInfo{
            .slot_id = mir::SlotId{static_cast<uint32_t>(i)},
            .type_id = type_ids[i],
            .type_info = slot_types[i],
        });
  }

  return slots;
}

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

  // Overwrite 4-state slots with X encoding
  for (const auto& slot : slots) {
    if (!IsFourStateType(slot.type_id, types)) {
      continue;
    }
    uint32_t field_index = context.GetDesignFieldIndex(slot.slot_id);
    auto* field_type = design_type->getElementType(field_index);
    auto* struct_type = llvm::cast<llvm::StructType>(field_type);
    uint32_t semantic_width = PackedBitWidth(types[slot.type_id], types);
    auto* slot_ptr =
        builder.CreateStructGEP(design_type, design_state, field_index);
    StoreFourStateX(builder, slot_ptr, struct_type, semantic_width);
  }
}

// Initialize ProcessStateN: zero everything, set design pointer, then
// overwrite 4-state frame places with X encoding
void InitializeProcessState(
    Context& context, llvm::Value* process_state, llvm::Value* design_state) {
  auto& builder = context.GetBuilder();
  auto* state_type = context.GetProcessStateType();
  auto* header_type = context.GetHeaderType();
  const auto& types = context.GetTypeArena();
  const auto& arena = context.GetMirArena();

  // Use aggregate zero for the entire state
  auto* zero = llvm::ConstantAggregateZero::get(state_type);
  builder.CreateStore(zero, process_state);

  // Set design pointer: header->design (field 1)
  auto* header_ptr = builder.CreateStructGEP(state_type, process_state, 0);
  auto* design_ptr_ptr = builder.CreateStructGEP(header_type, header_ptr, 1);
  builder.CreateStore(design_state, design_ptr_ptr);

  // Overwrite 4-state frame places with X encoding
  auto* frame_type = context.GetProcessFrameType();
  auto* frame_ptr = builder.CreateStructGEP(state_type, process_state, 1);
  const auto& frame_layout =
      context.GetLayout().processes[context.GetCurrentProcessIndex()].frame;

  for (mir::PlaceId place_id : frame_layout.places) {
    const auto& place = arena[place_id];
    if (!IsFourStateType(place.root.type, types)) {
      continue;
    }
    uint32_t field_index = context.GetFrameFieldIndex(place_id);
    auto* field_type = frame_type->getElementType(field_index);
    auto* struct_type = llvm::cast<llvm::StructType>(field_type);
    uint32_t semantic_width = PackedBitWidth(types[place.root.type], types);
    auto* field_ptr =
        builder.CreateStructGEP(frame_type, frame_ptr, field_index);
    StoreFourStateX(builder, field_ptr, struct_type, semantic_width);
  }
}

// Register tracked variables with runtime and emit snapshot call.
// O(N) complexity using pre-built SlotId -> field_index map from layout.
void RegisterAndSnapshotVariables(
    Context& context, const std::vector<VariableInfo>& variables,
    const std::vector<SlotTypeInfo>& slot_types, llvm::Value* design_state) {
  if (variables.empty()) {
    return;
  }

  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);
  auto* i1_ty = llvm::Type::getInt1Ty(llvm_ctx);
  auto* design_type = context.GetDesignStateType();

  for (const auto& var : variables) {
    if (var.slot_id >= slot_types.size()) {
      continue;
    }

    // Direct lookup using SlotId - no O(N) search needed
    auto slot_id = mir::SlotId{static_cast<uint32_t>(var.slot_id)};

    // Check if this slot exists in the layout
    // Use GetDesignFieldIndex which throws if slot is missing (catches
    // mismatches)
    uint32_t field_index = context.GetDesignFieldIndex(slot_id);
    auto* slot_ptr = builder.CreateStructGEP(
        design_type, design_state, field_index, "var_ptr");

    const auto& type_info = slot_types[var.slot_id];

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

// Release all string slots to prevent memory leaks
void ReleaseStringSlots(
    Context& context, const std::vector<SlotTypeInfo>& slot_types,
    llvm::Value* design_state) {
  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  auto* ptr_ty = llvm::PointerType::getUnqual(llvm_ctx);
  auto* design_type = context.GetDesignStateType();

  for (size_t slot_id = 0; slot_id < slot_types.size(); ++slot_id) {
    if (slot_types[slot_id].kind != VarTypeKind::kString) {
      continue;
    }

    auto mir_slot_id = mir::SlotId{static_cast<uint32_t>(slot_id)};
    uint32_t field_index = context.GetDesignFieldIndex(mir_slot_id);
    auto* slot_ptr =
        builder.CreateStructGEP(design_type, design_state, field_index);
    auto* val = builder.CreateLoad(ptr_ty, slot_ptr);
    builder.CreateCall(context.GetLyraStringRelease(), {val});
  }
}

}  // namespace

auto LowerMirToLlvm(const LoweringInput& input) -> LoweringResult {
  // Create LLVM context and module
  auto llvm_ctx = std::make_unique<llvm::LLVMContext>();
  auto module = std::make_unique<llvm::Module>("lyra_module", *llvm_ctx);

  // Build slot info list from input (includes TypeIds for LLVM type derivation)
  auto slot_info = BuildSlotInfoList(input.slot_types, input.slot_type_ids);

  // Build layout first (pure analysis, no LLVM IR emission)
  Layout layout = BuildLayout(
      *input.design, *input.mir_arena, *input.type_arena, slot_info, *llvm_ctx);

  // Create context with layout
  Context context(
      *input.design, *input.mir_arena, *input.type_arena, layout,
      std::move(llvm_ctx), std::move(module));

  auto& builder = context.GetBuilder();
  auto& ctx = context.GetLlvmContext();
  auto& mod = context.GetModule();

  // Generate process functions (use process_ids from layout for single source
  // of truth)
  std::vector<llvm::Function*> process_funcs;
  process_funcs.reserve(layout.process_ids.size());

  for (size_t i = 0; i < layout.process_ids.size(); ++i) {
    context.SetCurrentProcess(i);

    // Use the canonical process list from layout
    mir::ProcessId proc_id = layout.process_ids[i];
    const auto& mir_process = (*input.mir_arena)[proc_id];

    auto* func = GenerateProcessFunction(
        context, mir_process, std::format("process_{}", i));
    process_funcs.push_back(func);
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

  // Initialize runtime state (reset time tracker)
  builder.CreateCall(context.GetLyraInitRuntime());

  // Allocate and initialize all process states
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx);
  auto num_processes = static_cast<uint32_t>(process_funcs.size());
  std::vector<llvm::Value*> process_states;
  process_states.reserve(num_processes);

  for (size_t i = 0; i < process_funcs.size(); ++i) {
    context.SetCurrentProcess(i);

    auto* process_state = builder.CreateAlloca(
        context.GetProcessStateType(), nullptr,
        std::format("process_state_{}", i));

    InitializeProcessState(context, process_state, design_state);
    process_states.push_back(process_state);
  }

  // Build function pointer and state pointer arrays for multi-process call
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);
  auto* funcs_array = builder.CreateAlloca(
      ptr_ty, llvm::ConstantInt::get(i32_ty, num_processes), "funcs_array");
  auto* states_array = builder.CreateAlloca(
      ptr_ty, llvm::ConstantInt::get(i32_ty, num_processes), "states_array");

  for (uint32_t i = 0; i < num_processes; ++i) {
    auto* func_slot =
        builder.CreateGEP(ptr_ty, funcs_array, {builder.getInt32(i)});
    builder.CreateStore(process_funcs[i], func_slot);

    auto* state_slot =
        builder.CreateGEP(ptr_ty, states_array, {builder.getInt32(i)});
    builder.CreateStore(process_states[i], state_slot);
  }

  // Call multi-process scheduler
  builder.CreateCall(
      context.GetLyraRunSimulationMulti(),
      {funcs_array, states_array,
       llvm::ConstantInt::get(i32_ty, num_processes)});

  // Branch to exit block
  builder.CreateBr(exit_block);

  // Exit block: release strings, register/snapshot variables (if any), return 0
  builder.SetInsertPoint(exit_block);

  // Release all string locals to prevent memory leaks
  ReleaseStringSlots(context, input.slot_types, design_state);

  // Register and snapshot tracked variables for test framework inspection
  RegisterAndSnapshotVariables(
      context, input.variables, input.slot_types, design_state);

  // Report final simulation time for test harness
  if (input.emit_time_report) {
    builder.CreateCall(context.GetLyraReportTime());
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
