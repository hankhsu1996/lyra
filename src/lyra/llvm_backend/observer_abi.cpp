#include "lyra/llvm_backend/observer_abi.hpp"

#include <llvm/IR/Constants.h>
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
  return llvm::StructType::get(llvm_ctx, {ptr_ty, i32_ty, i32_ty, ptr_ty});
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
  auto* signal_id_offset = builder.CreateLoad(
      i32_ty, builder.CreateStructGEP(ctx_ty, observer_ctx_ptr, 2),
      "signal_id_offset");
  auto* unstable_offsets = builder.CreateLoad(
      ptr_ty, builder.CreateStructGEP(ctx_ty, observer_ctx_ptr, 3),
      "unstable_offsets");

  return {
      .this_ptr = this_ptr,
      .instance_id = instance_id,
      .signal_id_offset = signal_id_offset,
      .unstable_offsets = unstable_offsets,
  };
}

void EnterObserverSpecializationLocalContext(
    Context& context, mir::FunctionId func_id, llvm::Value* observer_ctx_ptr) {
  const auto& lowering = context.GetModuleFunctionLowering(func_id);
  context.SetSpecSlotLayout(lowering.spec_slot_layout);

  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  auto fields = LoadObserverContextFields(builder, llvm_ctx, observer_ctx_ptr);

  context.SetThisPointer(fields.this_ptr);
  context.SetDynamicInstanceId(fields.instance_id);
  context.SetSignalIdOffset(fields.signal_id_offset);
  context.SetUnstableSlotOffsetsPtr(fields.unstable_offsets);
  context.SetSlotAddressingMode(SlotAddressingMode::kSpecializationLocal);
}

auto GetObserverContextFieldValues(Context& context) -> LoadedObserverContext {
  auto& llvm_ctx = context.GetLlvmContext();
  auto* ptr_ty = llvm::PointerType::getUnqual(llvm_ctx);
  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);

  llvm::Value* this_ptr = context.GetThisPointer();
  llvm::Value* instance_id = context.GetDynamicInstanceId();
  llvm::Value* sig_offset = context.GetSignalIdOffset();
  llvm::Value* unstable_offsets = context.GetUnstableSlotOffsetsPtr();

  if (this_ptr == nullptr) {
    this_ptr = llvm::ConstantPointerNull::get(ptr_ty);
  }
  if (instance_id == nullptr) {
    instance_id = llvm::ConstantInt::get(i32_ty, 0);
  }
  if (sig_offset == nullptr) {
    sig_offset = llvm::ConstantInt::get(i32_ty, 0);
  }
  if (unstable_offsets == nullptr) {
    unstable_offsets = llvm::ConstantPointerNull::get(ptr_ty);
  }

  return {
      .this_ptr = this_ptr,
      .instance_id = instance_id,
      .signal_id_offset = sig_offset,
      .unstable_offsets = unstable_offsets,
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
  builder.CreateStore(
      fields.signal_id_offset, builder.CreateStructGEP(ctx_ty, ctx_alloca, 2));
  builder.CreateStore(
      fields.unstable_offsets, builder.CreateStructGEP(ctx_ty, ctx_alloca, 3));

  return ctx_alloca;
}

}  // namespace lyra::lowering::mir_to_llvm
