#include "lyra/llvm_backend/type_ops_store.hpp"

#include <cstdint>
#include <format>
#include <vector>

#include "llvm/IR/Constants.h"
#include "lyra/common/internal_error.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/type_ops_managed.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/place.hpp"

namespace lyra::lowering::mir_to_llvm {

auto GetLlvmStorageType(llvm::LLVMContext& ctx, uint32_t bit_width)
    -> llvm::Type* {
  if (bit_width <= 8) {
    return llvm::Type::getInt8Ty(ctx);
  }
  if (bit_width <= 16) {
    return llvm::Type::getInt16Ty(ctx);
  }
  if (bit_width <= 32) {
    return llvm::Type::getInt32Ty(ctx);
  }
  if (bit_width <= 64) {
    return llvm::Type::getInt64Ty(ctx);
  }
  return llvm::Type::getIntNTy(ctx, bit_width);
}

namespace {

// Build LLVM type for an unpacked struct TypeId
auto BuildUnpackedStructType(
    llvm::LLVMContext& ctx, const UnpackedStructInfo& info,
    const TypeArena& types) -> llvm::Type* {
  std::vector<llvm::Type*> field_types;
  field_types.reserve(info.fields.size());
  for (const auto& field : info.fields) {
    field_types.push_back(BuildLlvmTypeForTypeId(ctx, field.type, types));
  }
  return llvm::StructType::get(ctx, field_types);
}

}  // namespace

auto BuildLlvmTypeForTypeId(
    llvm::LLVMContext& ctx, TypeId type_id, const TypeArena& types)
    -> llvm::Type* {
  const Type& type = types[type_id];

  // String: opaque pointer
  if (type.Kind() == TypeKind::kString) {
    return llvm::PointerType::getUnqual(ctx);
  }

  // Unpacked struct
  if (type.Kind() == TypeKind::kUnpackedStruct) {
    return BuildUnpackedStructType(ctx, type.AsUnpackedStruct(), types);
  }

  // Packed types (integers, enums, etc.)
  if (IsPacked(type)) {
    auto bit_width = PackedBitWidth(type, types);
    if (IsPackedFourState(type, types)) {
      auto* plane_type = GetLlvmStorageType(ctx, bit_width);
      return llvm::StructType::get(ctx, {plane_type, plane_type});
    }
    return GetLlvmStorageType(ctx, bit_width);
  }

  throw common::InternalError(
      "BuildLlvmTypeForTypeId",
      std::format("unsupported type: {}", ToString(type)));
}

auto IsDesignPlace(Context& context, mir::PlaceId place_id) -> bool {
  const auto& place = context.GetMirArena()[place_id];
  return place.root.kind == mir::PlaceRoot::Kind::kDesign;
}

auto GetSignalId(Context& context, mir::PlaceId place_id) -> uint32_t {
  const auto& place = context.GetMirArena()[place_id];
  return static_cast<uint32_t>(place.root.id);
}

void StoreDesignWithNotify(
    Context& context, llvm::Value* new_value, llvm::Value* target_ptr,
    llvm::Type* storage_type, mir::PlaceId target_place) {
  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();

  // Compute byte size from storage type
  auto byte_size = static_cast<uint32_t>(
      context.GetModule().getDataLayout().getTypeAllocSize(storage_type));
  auto signal_id = GetSignalId(context, target_place);

  // Store new value to a temp alloca (notify helper reads from pointer)
  auto* temp = builder.CreateAlloca(storage_type, nullptr, "notify_tmp");
  builder.CreateStore(new_value, temp);

  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);
  builder.CreateCall(
      context.GetLyraStorePacked(),
      {context.GetEnginePointer(), target_ptr, temp,
       llvm::ConstantInt::get(i32_ty, byte_size),
       llvm::ConstantInt::get(i32_ty, signal_id)});
}

void Destroy(Context& context, llvm::Value* ptr, TypeId type_id) {
  auto& builder = context.GetBuilder();
  const auto& types = context.GetTypeArena();
  const Type& type = types[type_id];

  switch (type.Kind()) {
    case TypeKind::kString: {
      auto* ptr_ty = llvm::PointerType::getUnqual(context.GetLlvmContext());
      auto* val = builder.CreateLoad(ptr_ty, ptr, "destroy.str");
      builder.CreateCall(context.GetLyraStringRelease(), {val});
      return;
    }
    case TypeKind::kDynamicArray:
    case TypeKind::kQueue: {
      auto* ptr_ty = llvm::PointerType::getUnqual(context.GetLlvmContext());
      auto* val = builder.CreateLoad(ptr_ty, ptr, "destroy.da");
      builder.CreateCall(context.GetLyraDynArrayRelease(), {val});
      return;
    }
    case TypeKind::kUnpackedStruct:
      if (NeedsDestroy(type_id, types)) {
        DestroyStructFields(context, ptr, type_id);
      }
      return;
    default:
      return;  // Value types: no-op
  }
}

void DestroyStructFields(
    Context& context, llvm::Value* ptr, TypeId struct_type_id) {
  auto& builder = context.GetBuilder();
  const auto& types = context.GetTypeArena();
  const Type& struct_type = types[struct_type_id];
  const auto& struct_info = struct_type.AsUnpackedStruct();

  llvm::Type* llvm_struct_type =
      BuildLlvmTypeForTypeId(context.GetLlvmContext(), struct_type_id, types);

  for (size_t i = 0; i < struct_info.fields.size(); ++i) {
    const auto& field = struct_info.fields[i];
    auto field_idx = static_cast<unsigned>(i);
    llvm::Value* field_ptr =
        builder.CreateStructGEP(llvm_struct_type, ptr, field_idx);
    Destroy(context, field_ptr, field.type);
  }
}

}  // namespace lyra::lowering::mir_to_llvm
