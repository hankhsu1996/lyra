#include "lyra/lowering/mir_to_llvm/lower.hpp"

#include <cstddef>
#include <optional>
#include <vector>

#include "llvm/IR/Constants.h"
#include "llvm/Support/raw_ostream.h"
#include "lyra/lowering/mir_to_llvm/context.hpp"
#include "lyra/lowering/mir_to_llvm/process.hpp"
#include "lyra/mir/place.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

// Find the PlaceId for a design slot by scanning the arena
auto FindDesignSlotPlaceId(const mir::Arena& arena, size_t slot_id)
    -> std::optional<mir::PlaceId> {
  for (uint32_t i = 0; i < arena.PlaceCount(); ++i) {
    mir::PlaceId place_id{i};
    const auto& place = arena[place_id];
    if (place.root.kind == mir::PlaceRoot::Kind::kDesign &&
        place.root.id == static_cast<int>(slot_id) &&
        place.projections.empty()) {
      return place_id;
    }
  }
  return std::nullopt;
}

// Pre-create allocas for ALL design slots and initialize to SV defaults.
// Returns a vector of allocas indexed by slot_id.
auto InitializeAllSlots(
    Context& context, const std::vector<SlotTypeInfo>& slot_types)
    -> std::vector<llvm::AllocaInst*> {
  std::vector<llvm::AllocaInst*> allocas(slot_types.size(), nullptr);
  if (slot_types.empty()) {
    return allocas;
  }

  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  const auto& arena = context.GetMirArena();

  for (size_t slot_id = 0; slot_id < slot_types.size(); ++slot_id) {
    auto place_id_opt = FindDesignSlotPlaceId(arena, slot_id);
    if (!place_id_opt) {
      continue;
    }

    llvm::AllocaInst* alloca = context.GetOrCreatePlaceStorage(*place_id_opt);
    allocas[slot_id] = alloca;

    // Initialize to SV default (0 for integral, 0.0 for real)
    const auto& type_info = slot_types[slot_id];
    if (type_info.kind == VarTypeKind::kReal) {
      builder.CreateStore(
          llvm::ConstantFP::get(llvm::Type::getDoubleTy(llvm_ctx), 0.0),
          alloca);
    } else {
      builder.CreateStore(
          llvm::ConstantInt::get(alloca->getAllocatedType(), 0), alloca);
    }
  }
  return allocas;
}

// Register tracked variables with runtime and emit snapshot call.
void RegisterAndSnapshotVariables(
    Context& context, const std::vector<VariableInfo>& variables,
    const std::vector<SlotTypeInfo>& slot_types,
    const std::vector<llvm::AllocaInst*>& allocas) {
  if (variables.empty()) {
    return;
  }

  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);
  auto* i1_ty = llvm::Type::getInt1Ty(llvm_ctx);

  for (const auto& var : variables) {
    if (var.slot_id >= allocas.size() || allocas[var.slot_id] == nullptr) {
      continue;
    }

    auto* alloca = allocas[var.slot_id];
    const auto& type_info = slot_types[var.slot_id];

    auto* name_ptr = builder.CreateGlobalStringPtr(var.name);
    auto* kind_val =
        llvm::ConstantInt::get(i32_ty, static_cast<int32_t>(type_info.kind));
    auto* width_val = llvm::ConstantInt::get(i32_ty, type_info.width);
    auto* signed_val =
        llvm::ConstantInt::get(i1_ty, type_info.is_signed ? 1 : 0);
    builder.CreateCall(
        context.GetLyraRegisterVar(),
        {name_ptr, alloca, kind_val, width_val, signed_val});
  }

  builder.CreateCall(context.GetLyraSnapshotVars());
}

}  // namespace

auto LowerMirToLlvm(const LoweringInput& input) -> LoweringResult {
  Context context(*input.design, *input.mir_arena, *input.type_arena);

  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  auto& module = context.GetModule();

  // Create main function: int main()
  auto* main_type =
      llvm::FunctionType::get(llvm::Type::getInt32Ty(llvm_ctx), false);
  auto* main_func = llvm::Function::Create(
      main_type, llvm::Function::ExternalLinkage, "main", &module);

  auto* entry = llvm::BasicBlock::Create(llvm_ctx, "entry", main_func);
  auto* exit_block = llvm::BasicBlock::Create(llvm_ctx, "exit", main_func);
  builder.SetInsertPoint(entry);

  // Initialize ALL design slots to SV defaults
  auto allocas = InitializeAllSlots(context, input.slot_types);

  // Collect all initial processes
  std::vector<const mir::Process*> initial_processes;
  for (const auto& element : input.design->elements) {
    if (!std::holds_alternative<mir::Module>(element)) {
      continue;
    }
    const auto& mir_module = std::get<mir::Module>(element);
    for (mir::ProcessId proc_id : mir_module.processes) {
      const auto& process = (*input.mir_arena)[proc_id];
      if (process.kind == mir::ProcessKind::kOnce) {
        initial_processes.push_back(&process);
      }
    }
  }

  // Lower processes with proper chaining: each process's continuation
  // is the start of the next process, or exit_block for the last one.
  for (size_t i = 0; i < initial_processes.size(); ++i) {
    bool is_last = (i == initial_processes.size() - 1);
    auto* continuation =
        is_last ? exit_block
                : llvm::BasicBlock::Create(llvm_ctx, "process_cont", main_func);
    LowerProcess(context, *initial_processes[i], continuation);
    if (!is_last) {
      builder.SetInsertPoint(continuation);
    }
  }

  // If no processes, entry needs to branch somewhere
  if (initial_processes.empty()) {
    builder.CreateBr(exit_block);
  }

  // Exit block: register/snapshot variables (if any) then return 0
  builder.SetInsertPoint(exit_block);

  // Register and snapshot tracked variables for test framework inspection
  RegisterAndSnapshotVariables(
      context, input.variables, input.slot_types, allocas);
  builder.CreateRet(llvm::ConstantInt::get(llvm_ctx, llvm::APInt(32, 0)));

  auto [ctx, mod] = context.TakeOwnership();
  return LoweringResult{
      .context = std::move(ctx),
      .module = std::move(mod),
  };
}

auto DumpLlvmIr(const LoweringResult& result) -> std::string {
  std::string ir;
  llvm::raw_string_ostream stream(ir);
  result.module->print(stream, nullptr);
  return ir;
}

}  // namespace lyra::lowering::mir_to_llvm
