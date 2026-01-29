#include "lyra/llvm_backend/default_init.hpp"

#include <cstddef>
#include <cstdint>

#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
#include <llvm/Support/Casting.h>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/union_storage.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

// Store X-encoded value to a 4-state pointer.
// X encoding: {value=0, unknown=semantic_mask}
// semantic_mask has 1s in the low semantic_width bits.
//
// CONTRACT: ptr must point to storage with the exact layout of struct_type,
// i.e., {iN, iN} where N is the storage width for the packed bit_width.
// This matches the 4-state storage layout used throughout the LLVM backend.
void EmitStoreFourStateX(
    Context& ctx, llvm::Value* ptr, llvm::StructType* struct_type,
    uint32_t semantic_width) {
  auto& builder = ctx.GetBuilder();
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

// Emit memset(ptr, 0, size) for zero-initializing storage.
void EmitMemsetZero(Context& ctx, llvm::Value* ptr, llvm::Type* llvm_type) {
  auto& builder = ctx.GetBuilder();
  auto& module = ctx.GetModule();

  auto byte_size = module.getDataLayout().getTypeAllocSize(llvm_type);

  // Use IRBuilder::CreateMemSet for correct intrinsic signature handling
  builder.CreateMemSet(ptr, builder.getInt8(0), byte_size, llvm::MaybeAlign());
}

}  // namespace

void EmitSVDefaultInit(Context& ctx, llvm::Value* ptr, TypeId type_id) {
  auto& builder = ctx.GetBuilder();
  auto& llvm_ctx = ctx.GetLlvmContext();
  const auto& types = ctx.GetTypeArena();
  const Type& type = types[type_id];

  switch (type.Kind()) {
    case TypeKind::kIntegral: {
      uint32_t bit_width = type.AsIntegral().bit_width;
      if (type.AsIntegral().is_four_state) {
        auto* struct_type = ctx.GetPlaceLlvmType4State(bit_width);
        EmitStoreFourStateX(ctx, ptr, struct_type, bit_width);
      } else {
        auto* llvm_type = GetLlvmStorageType(llvm_ctx, bit_width);
        auto* zero = llvm::Constant::getNullValue(llvm_type);
        builder.CreateStore(zero, ptr);
      }
      return;
    }

    case TypeKind::kReal: {
      auto* llvm_type = llvm::Type::getDoubleTy(llvm_ctx);
      auto* zero = llvm::ConstantFP::get(llvm_type, 0.0);
      builder.CreateStore(zero, ptr);
      return;
    }

    case TypeKind::kShortReal: {
      auto* llvm_type = llvm::Type::getFloatTy(llvm_ctx);
      auto* zero = llvm::ConstantFP::get(llvm_type, 0.0);
      builder.CreateStore(zero, ptr);
      return;
    }

    case TypeKind::kString:
    case TypeKind::kDynamicArray:
    case TypeKind::kQueue: {
      auto* ptr_ty = llvm::PointerType::getUnqual(llvm_ctx);
      auto* null_ptr = llvm::ConstantPointerNull::get(ptr_ty);
      builder.CreateStore(null_ptr, ptr);
      return;
    }

    case TypeKind::kPackedArray:
    case TypeKind::kPackedStruct:
    case TypeKind::kEnum: {
      uint32_t width = PackedBitWidth(type, types);
      if (IsPackedFourState(type, types)) {
        auto* struct_type = ctx.GetPlaceLlvmType4State(width);
        EmitStoreFourStateX(ctx, ptr, struct_type, width);
      } else {
        auto* llvm_type = GetLlvmStorageType(llvm_ctx, width);
        auto* zero = llvm::Constant::getNullValue(llvm_type);
        builder.CreateStore(zero, ptr);
      }
      return;
    }

    case TypeKind::kUnpackedStruct: {
      auto llvm_type_result = BuildLlvmTypeForTypeId(ctx, type_id);
      if (!llvm_type_result) {
        throw common::InternalError(
            "EmitSVDefaultInit", "failed to build LLVM type for struct");
      }
      auto* struct_llvm_type = llvm::cast<llvm::StructType>(*llvm_type_result);
      const auto& info = type.AsUnpackedStruct();
      for (size_t i = 0; i < info.fields.size(); ++i) {
        auto* field_ptr = builder.CreateStructGEP(
            struct_llvm_type, ptr, static_cast<unsigned>(i));
        EmitSVDefaultInit(ctx, field_ptr, info.fields[i].type);
      }
      return;
    }

    case TypeKind::kUnpackedArray: {
      auto llvm_type_result = BuildLlvmTypeForTypeId(ctx, type_id);
      if (!llvm_type_result) {
        throw common::InternalError(
            "EmitSVDefaultInit", "failed to build LLVM type for array");
      }
      auto* array_llvm_type = *llvm_type_result;
      const auto& info = type.AsUnpackedArray();
      // NOTE: i=0..N-1 is memory order, not SV index order. For default init,
      // order doesn't matter since all elements get the same value.
      for (uint32_t i = 0; i < info.range.Size(); ++i) {
        auto* elem_ptr = builder.CreateConstGEP2_32(array_llvm_type, ptr, 0, i);
        EmitSVDefaultInit(ctx, elem_ptr, info.element_type);
      }
      return;
    }

    case TypeKind::kUnpackedUnion: {
      // TODO(hankhsu): 4-state unions not yet supported. Zero-fill for now.
      auto llvm_type_result = BuildLlvmTypeForTypeId(ctx, type_id);
      if (!llvm_type_result) {
        throw common::InternalError(
            "EmitSVDefaultInit", "failed to build LLVM type for union");
      }
      EmitMemsetZero(ctx, ptr, *llvm_type_result);
      return;
    }

    default:
      throw common::InternalError(
          "EmitSVDefaultInit", "unsupported type for default initialization");
  }
}

}  // namespace lyra::lowering::mir_to_llvm
