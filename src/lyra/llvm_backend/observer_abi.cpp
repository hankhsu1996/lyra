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
  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);
  return llvm::StructType::get(llvm_ctx, {ptr_ty, i32_ty});
}

auto LoadObserverContextFields(
    llvm::IRBuilder<>& builder, llvm::LLVMContext& llvm_ctx,
    llvm::Value* observer_ctx_ptr) -> LoadedObserverContext {
  auto* ctx_ty = GetObserverContextStructType(llvm_ctx);
  auto* ptr_ty = llvm::PointerType::getUnqual(llvm_ctx);
  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);

  auto* this_ptr = builder.CreateLoad(
      ptr_ty, builder.CreateStructGEP(ctx_ty, observer_ctx_ptr, 0), "this_ptr");
  auto* instance_id = builder.CreateLoad(
      i32_ty, builder.CreateStructGEP(ctx_ty, observer_ctx_ptr, 1),
      "instance_id");

  return {
      .this_ptr = this_ptr,
      .instance_id = instance_id,
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
  context.SetObserverInstanceId(fields.instance_id);
  context.SetSlotAddressingMode(SlotAddressingMode::kSpecializationLocal);
}

auto GetObserverContextFieldValues(Context& context) -> LoadedObserverContext {
  auto& llvm_ctx = context.GetLlvmContext();
  auto* ptr_ty = llvm::PointerType::getUnqual(llvm_ctx);
  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);

  llvm::Value* this_ptr = context.GetThisPointer();
  llvm::Value* instance_id = context.GetObserverInstanceId();

  if (this_ptr == nullptr) {
    this_ptr = llvm::ConstantPointerNull::get(ptr_ty);
  }
  if (instance_id == nullptr) {
    // Observer ABI bridge (deferred migration): derive numeric instance
    // token from instance pointer for strobe/monitor registration which
    // still uses the i32 observer context ABI. This derivation is
    // observer-specific -- normal module/process code must NOT use
    // numeric instance tokens. Will be removed when observer ABI
    // migrates to pointer-based identity.
    auto* inst = context.GetInstancePointer();
    if (inst != nullptr) {
      instance_id = context.GetBuilder().CreateCall(
          context.GetLyraGetInstanceOrdinal(),
          {context.GetEnginePointer(), inst}, "observer_inst_id");
    } else {
      instance_id = llvm::ConstantInt::get(i32_ty, 0);
    }
  }

  return {
      .this_ptr = this_ptr,
      .instance_id = instance_id,
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
      fields.instance_id, builder.CreateStructGEP(ctx_ty, ctx_alloca, 1));

  return ctx_alloca;
}

}  // namespace lyra::lowering::mir_to_llvm
