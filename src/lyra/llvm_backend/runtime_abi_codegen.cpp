#include "lyra/llvm_backend/runtime_abi_codegen.hpp"

#include <array>
#include <cstdint>

#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Type.h>

#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/cu_facts.hpp"
#include "lyra/llvm_backend/layout/layout.hpp"
#include "lyra/runtime/runtime_instance.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

// ABI struct field indices. Private to this file.
// Must match the C++ LyraRuntimeAbi in runtime_abi.hpp.
constexpr unsigned kAbiFieldCount = 42;
constexpr unsigned kAbiFieldInstancePtrs = 27;

}  // namespace

auto GetRuntimeAbiStructType(llvm::LLVMContext& ctx) -> llvm::StructType* {
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx);

  auto* i8_ty = llvm::Type::getInt8Ty(ctx);
  std::array<llvm::Type*, kAbiFieldCount> fields = {
      i32_ty,
      ptr_ty,
      i32_ty,
      ptr_ty,
      i32_ty,
      ptr_ty,
      i32_ty,
      ptr_ty,
      i32_ty,
      ptr_ty,
      i32_ty,
      ptr_ty,
      i32_ty,
      ptr_ty,
      i32_ty,
      i32_ty,
      ptr_ty,
      i32_ty,
      ptr_ty,
      i32_ty,
      ptr_ty,
      i32_ty,
      ptr_ty,
      ptr_ty,
      i32_ty,
      i32_ty,
      ptr_ty,
      ptr_ty,
      i32_ty,
      ptr_ty,
      i32_ty,
      i32_ty,
      i32_ty,
      i32_ty,
      i8_ty,
      // A2: deferred assertion site metadata (ptr, i32 count, i32 pad)
      ptr_ty,
      i32_ty,
      i32_ty,
      // L8a: named event count (i32 count, i32 pad)
      i32_ty,
      i32_ty,
      // v25: fs_root (ptr)
      ptr_ty,
      // v26: construction_result (ptr)
      ptr_ty,
  };
  return llvm::StructType::get(ctx, fields, false);
}

auto EmitLoadAbiInstancePtr(
    Context& context, llvm::Value* abi_ptr, uint32_t instance_id)
    -> llvm::Value* {
  auto& builder = context.GetBuilder();
  auto& ctx = context.GetLlvmContext();
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx);

  auto* abi_struct_type = GetRuntimeAbiStructType(ctx);

  // Load instance_ptrs array pointer from ABI struct.
  auto* ptrs_field_ptr = builder.CreateStructGEP(
      abi_struct_type, abi_ptr, kAbiFieldInstancePtrs, "abi_instance_ptrs_ptr");
  auto* ptrs_array =
      builder.CreateLoad(ptr_ty, ptrs_field_ptr, "instance_ptrs");

  // Index into instance_ptrs[instance_id].
  auto* inst_ptr_ptr = builder.CreateGEP(
      ptr_ty, ptrs_array, builder.getInt32(instance_id), "inst_ptr_ptr");
  return builder.CreateLoad(ptr_ty, inst_ptr_ptr, "inst_ptr");
}

auto EmitInstanceOwnedByteAddress(
    Context& context, const CuFacts& facts, llvm::Value* instance_ptr,
    common::InstanceByteOffset rel_off) -> llvm::Value* {
  auto& builder = context.GetBuilder();
  auto& ctx = context.GetLlvmContext();
  const auto& layout = *facts.layout;
  auto* i8_ty = llvm::Type::getInt8Ty(ctx);
  auto* i64_ty = llvm::Type::getInt64Ty(ctx);
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx);

  // GEP to RuntimeInstance::storage (field kStorage = 2).
  auto* storage_ptr = builder.CreateStructGEP(
      layout.runtime_instance_type, instance_ptr,
      static_cast<unsigned>(runtime::RuntimeInstanceField::kStorage),
      "inst_storage");

  // Load storage.inline_size (subfield kInlineSize = 1).
  auto* inline_size_ptr = builder.CreateStructGEP(
      layout.runtime_instance_storage_type, storage_ptr,
      static_cast<unsigned>(runtime::RuntimeInstanceStorageField::kInlineSize),
      "inline_size_ptr");
  auto* inline_size =
      builder.CreateLoad(i64_ty, inline_size_ptr, "inline_size");

  // Runtime compare: is the offset within inline storage?
  auto* offset_val = builder.getInt64(rel_off.value);
  auto* in_inline = builder.CreateICmpULT(offset_val, inline_size, "in_inline");

  // Inline path: inline_base + rel_off.
  auto* inline_base_ptr = builder.CreateStructGEP(
      layout.runtime_instance_storage_type, storage_ptr,
      static_cast<unsigned>(runtime::RuntimeInstanceStorageField::kInlineBase),
      "inline_base_ptr");
  auto* inline_base =
      builder.CreateLoad(ptr_ty, inline_base_ptr, "inline_base");
  auto* inline_addr =
      builder.CreateGEP(i8_ty, inline_base, offset_val, "inline_addr");

  // Appendix path: appendix_base + (rel_off - inline_size).
  auto* appendix_base_ptr = builder.CreateStructGEP(
      layout.runtime_instance_storage_type, storage_ptr,
      static_cast<unsigned>(
          runtime::RuntimeInstanceStorageField::kAppendixBase),
      "appendix_base_ptr");
  auto* appendix_base =
      builder.CreateLoad(ptr_ty, appendix_base_ptr, "appendix_base");
  auto* appendix_off =
      builder.CreateSub(offset_val, inline_size, "appendix_off");
  auto* appendix_addr =
      builder.CreateGEP(i8_ty, appendix_base, appendix_off, "appendix_addr");

  // Merge with select.
  return builder.CreateSelect(
      in_inline, inline_addr, appendix_addr, "storage_addr");
}

}  // namespace lyra::lowering::mir_to_llvm
