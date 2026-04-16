#include "lyra/llvm_backend/observer_abi.hpp"

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>

#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/handle.hpp"

namespace lyra::lowering::mir_to_llvm {

auto GetObserverContextStructType(llvm::LLVMContext& llvm_ctx)
    -> llvm::StructType* {
  auto* ptr_ty = llvm::PointerType::getUnqual(llvm_ctx);
  return llvm::StructType::get(llvm_ctx, {ptr_ty, ptr_ty});
}

auto LoadObserverContextFields(
    llvm::IRBuilder<>& builder, llvm::LLVMContext& llvm_ctx,
    llvm::Value* observer_ctx_ptr) -> LoadedObserverContext {
  auto* ctx_ty = GetObserverContextStructType(llvm_ctx);
  auto* ptr_ty = llvm::PointerType::getUnqual(llvm_ctx);

  auto* this_ptr = builder.CreateLoad(
      ptr_ty, builder.CreateStructGEP(ctx_ty, observer_ctx_ptr, 0), "this_ptr");
  auto* instance = builder.CreateLoad(
      ptr_ty, builder.CreateStructGEP(ctx_ty, observer_ctx_ptr, 1),
      "observer_instance");

  return {
      .this_ptr = this_ptr,
      .instance = instance,
  };
}

void EnterObserverSpecializationLocalContext(
    Context& context, [[maybe_unused]] mir::FunctionId func_id,
    llvm::Value* observer_ctx_ptr) {
  // spec_slot_info and connection_notification_mask are already set on
  // Context by SpecLocalScope at session start. All body functions share
  // the same body-level spec context.
  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  auto fields = LoadObserverContextFields(builder, llvm_ctx, observer_ctx_ptr);

  context.SetThisPointer(fields.this_ptr);
  context.SetObserverInstancePtr(fields.instance);
  context.SetSlotAddressingMode(SlotAddressingMode::kSpecializationLocal);
}

auto GetObserverContextFieldValues(Context& context) -> LoadedObserverContext {
  auto& llvm_ctx = context.GetLlvmContext();
  auto* ptr_ty = llvm::PointerType::getUnqual(llvm_ctx);

  llvm::Value* this_ptr = context.GetThisPointer();
  llvm::Value* instance = context.GetObserverInstancePtr();

  if (this_ptr == nullptr) {
    this_ptr = llvm::ConstantPointerNull::get(ptr_ty);
  }
  if (instance == nullptr) {
    // No observer context active. Use the current instance pointer
    // directly if available (module-scoped caller), or nullptr for
    // design-global callers.
    auto* inst = context.GetInstancePointer();
    if (inst != nullptr) {
      instance = inst;
    } else {
      instance = llvm::ConstantPointerNull::get(ptr_ty);
    }
  }

  return {
      .this_ptr = this_ptr,
      .instance = instance,
  };
}

auto MaterializeObserverContext(Context& context) -> llvm::Value* {
  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();

  auto* ctx_ty = GetObserverContextStructType(llvm_ctx);
  auto* ctx_alloca = builder.CreateAlloca(ctx_ty, nullptr, "obs_ctx");

  auto fields = GetObserverContextFieldValues(context);

  builder.CreateStore(
      fields.this_ptr, builder.CreateStructGEP(ctx_ty, ctx_alloca, 0));
  builder.CreateStore(
      fields.instance, builder.CreateStructGEP(ctx_ty, ctx_alloca, 1));

  return ctx_alloca;
}

}  // namespace lyra::lowering::mir_to_llvm
