#include "lyra/llvm_backend/type_ops/default_init.hpp"

#include <cstddef>
#include <cstdint>
#include <optional>

#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Metadata.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
#include <llvm/Support/Alignment.h>
#include <llvm/Support/Casting.h>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/canonical_plane_write.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/cu_facts.hpp"
#include "lyra/llvm_backend/layout/storage_contract.hpp"
#include "lyra/llvm_backend/layout/union_storage.hpp"
#include "lyra/llvm_backend/storage_boundary.hpp"
#include "lyra/llvm_backend/value_repr.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

constexpr uint32_t kSmallArrayUnrollThreshold = 8;

void EmitFourStateXInit(Context& ctx, llvm::Value* ptr, uint32_t bit_width) {
  EmitStoreFourStateX(ctx.GetBuilder(), ptr, bit_width);
}

void EmitMemsetZeroLocal(
    Context& ctx, llvm::Value* ptr, llvm::Type* llvm_type) {
  EmitMemsetZero(ctx, ptr, llvm_type);
}

void EmitSVDefaultInitImpl(
    Context& ctx, const CuFacts& facts, llvm::Value* ptr, TypeId type_id,
    bool already_zeroed, bool is_canonical) {
  auto& builder = ctx.GetBuilder();
  auto& llvm_ctx = ctx.GetLlvmContext();
  const auto& types = *facts.types;
  const Type& type = types[type_id];

  switch (type.Kind()) {
    case TypeKind::kIntegral: {
      uint32_t bit_width = type.AsIntegral().bit_width;
      if (IsPackedFourState(facts, type)) {
        if (already_zeroed) {
          EmitStoreUnknownMask(builder, ptr, bit_width);
        } else {
          EmitFourStateXInit(ctx, ptr, bit_width);
        }
      } else {
        if (!already_zeroed) {
          if (is_canonical) {
            EmitCanonicalPlaneWrite(
                builder, ptr, 0,
                {.kind = PlaneConstantKind::kZero,
                 .semantic_bit_width = bit_width,
                 .storage_byte_size = GetStorageByteSize(bit_width)});
          } else {
            auto* llvm_type = GetBackingLlvmType(llvm_ctx, bit_width);
            builder.CreateStore(llvm::Constant::getNullValue(llvm_type), ptr);
          }
        }
      }
      return;
    }

    case TypeKind::kReal: {
      if (!already_zeroed) {
        auto* llvm_type = llvm::Type::getDoubleTy(llvm_ctx);
        auto* zero = llvm::ConstantFP::get(llvm_type, 0.0);
        builder.CreateStore(zero, ptr);
      }
      return;
    }

    case TypeKind::kShortReal: {
      if (!already_zeroed) {
        auto* llvm_type = llvm::Type::getFloatTy(llvm_ctx);
        auto* zero = llvm::ConstantFP::get(llvm_type, 0.0);
        builder.CreateStore(zero, ptr);
      }
      return;
    }

    case TypeKind::kChandle:
    case TypeKind::kString:
    case TypeKind::kDynamicArray:
    case TypeKind::kQueue:
    case TypeKind::kAssociativeArray: {
      if (!already_zeroed) {
        auto* ptr_ty = llvm::PointerType::getUnqual(llvm_ctx);
        auto* null_ptr = llvm::ConstantPointerNull::get(ptr_ty);
        builder.CreateStore(null_ptr, ptr);
      }
      return;
    }

    case TypeKind::kPackedArray:
    case TypeKind::kPackedStruct:
    case TypeKind::kEnum: {
      uint32_t width = PackedBitWidth(type, types);
      if (IsPackedFourState(facts, type)) {
        if (already_zeroed) {
          EmitStoreUnknownMask(builder, ptr, width);
        } else {
          EmitFourStateXInit(ctx, ptr, width);
        }
      } else {
        if (!already_zeroed) {
          if (is_canonical) {
            EmitCanonicalPlaneWrite(
                builder, ptr, 0,
                {.kind = PlaneConstantKind::kZero,
                 .semantic_bit_width = width,
                 .storage_byte_size = GetStorageByteSize(width)});
          } else {
            auto* llvm_type = GetBackingLlvmType(llvm_ctx, width);
            builder.CreateStore(llvm::Constant::getNullValue(llvm_type), ptr);
          }
        }
      }
      return;
    }

    case TypeKind::kUnpackedStruct: {
      auto llvm_type_result = BuildLlvmTypeForTypeId(ctx, facts, type_id);
      if (!llvm_type_result) {
        throw common::InternalError(
            "EmitSVDefaultInitImpl", "failed to build LLVM type for struct");
      }
      auto* struct_llvm_type = llvm::cast<llvm::StructType>(*llvm_type_result);
      const auto& info = type.AsUnpackedStruct();
      for (size_t i = 0; i < info.fields.size(); ++i) {
        auto* field_ptr = builder.CreateStructGEP(
            struct_llvm_type, ptr, static_cast<unsigned>(i));
        EmitSVDefaultInitImpl(
            ctx, facts, field_ptr, info.fields[i].type, already_zeroed,
            is_canonical);
      }
      return;
    }

    case TypeKind::kUnpackedArray: {
      auto llvm_type_result = BuildLlvmTypeForTypeId(ctx, facts, type_id);
      if (!llvm_type_result) {
        throw common::InternalError(
            "EmitSVDefaultInitImpl", "failed to build LLVM type for array");
      }
      auto* array_llvm_type = llvm::cast<llvm::ArrayType>(*llvm_type_result);
      const auto& info = type.AsUnpackedArray();
      uint32_t count = info.range.Size();

      if (count == 0) return;

      if (!IsFourState(facts, info.element_type)) {
        if (!already_zeroed) {
          EmitMemsetZeroLocal(ctx, ptr, array_llvm_type);
        }
        return;
      }

      if (!already_zeroed) {
        EmitMemsetZeroLocal(ctx, ptr, array_llvm_type);
      }

      const Type& elem_type = types[info.element_type];

      if (count <= kSmallArrayUnrollThreshold) {
        for (uint32_t i = 0; i < count; ++i) {
          auto* elem_ptr =
              builder.CreateConstGEP2_32(array_llvm_type, ptr, 0, i);

          if (elem_type.Kind() == TypeKind::kIntegral &&
              IsPackedFourState(facts, elem_type)) {
            uint32_t bit_width = elem_type.AsIntegral().bit_width;
            EmitStoreUnknownMask(builder, elem_ptr, bit_width);
          } else {
            EmitSVDefaultInitImpl(
                ctx, facts, elem_ptr, info.element_type, true, is_canonical);
          }
        }
        return;
      }

      auto* func = builder.GetInsertBlock()->getParent();
      auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);

      auto* preheader = builder.GetInsertBlock();
      auto* header = llvm::BasicBlock::Create(llvm_ctx, "arr.init.hdr", func);
      auto* body = llvm::BasicBlock::Create(llvm_ctx, "arr.init.body", func);
      auto* latch = llvm::BasicBlock::Create(llvm_ctx, "arr.init.latch", func);
      auto* exit_bb = llvm::BasicBlock::Create(llvm_ctx, "arr.init.exit", func);

      builder.CreateBr(header);

      builder.SetInsertPoint(header);
      auto* phi = builder.CreatePHI(i32_ty, 2, "arr.init.idx");
      phi->addIncoming(llvm::ConstantInt::get(i32_ty, 0), preheader);
      auto* cond = builder.CreateICmpULT(
          phi, llvm::ConstantInt::get(i32_ty, count), "arr.init.cmp");
      builder.CreateCondBr(cond, body, exit_bb);

      builder.SetInsertPoint(body);
      auto* elem_ptr = builder.CreateGEP(
          array_llvm_type, ptr, {builder.getInt32(0), phi}, "arr.init.elem");

      if (elem_type.Kind() == TypeKind::kIntegral &&
          IsPackedFourState(facts, elem_type)) {
        uint32_t bit_width = elem_type.AsIntegral().bit_width;
        EmitStoreUnknownMask(builder, elem_ptr, bit_width);
      } else {
        EmitSVDefaultInitImpl(
            ctx, facts, elem_ptr, info.element_type, true, is_canonical);
      }
      builder.CreateBr(latch);

      builder.SetInsertPoint(latch);
      auto* next_idx = builder.CreateAdd(
          phi, llvm::ConstantInt::get(i32_ty, 1), "arr.init.next");
      phi->addIncoming(next_idx, latch);

      auto* loop_id = llvm::MDNode::getDistinct(llvm_ctx, {nullptr});
      loop_id->replaceOperandWith(0, loop_id);
      auto* unroll_disable = llvm::MDNode::get(
          llvm_ctx,
          {llvm::MDString::get(llvm_ctx, "llvm.loop.unroll.disable")});
      auto* loop_md = llvm::MDNode::get(llvm_ctx, {loop_id, unroll_disable});
      auto* backedge = builder.CreateBr(header);
      backedge->setMetadata(llvm::LLVMContext::MD_loop, loop_md);

      builder.SetInsertPoint(exit_bb);
      return;
    }

    case TypeKind::kUnpackedUnion: {
      if (!already_zeroed) {
        auto llvm_type_result = BuildLlvmTypeForTypeId(ctx, facts, type_id);
        if (!llvm_type_result) {
          throw common::InternalError(
              "EmitSVDefaultInitImpl", "failed to build LLVM type for union");
        }
        EmitMemsetZeroLocal(ctx, ptr, *llvm_type_result);
      }
      return;
    }

    default:
      throw common::InternalError(
          "EmitSVDefaultInitImpl",
          "unsupported type for default initialization");
  }
}

}  // namespace

void EmitSVDefaultInit(
    Context& ctx, const CuFacts& facts, llvm::Value* ptr, TypeId type_id) {
  EmitSVDefaultInitImpl(ctx, facts, ptr, type_id, false, false);
}

void EmitSVDefaultInitAfterZero(
    Context& ctx, const CuFacts& facts, llvm::Value* ptr, TypeId type_id) {
  EmitSVDefaultInitImpl(ctx, facts, ptr, type_id, true, true);
}

void EmitMemsetZero(
    Context& ctx, llvm::Value* dst_ptr, llvm::Type* pointee_ty,
    std::optional<llvm::Align> align) {
  auto& builder = ctx.GetBuilder();
  auto& module = ctx.GetModule();
  const llvm::DataLayout& dl = module.getDataLayout();

  uint64_t byte_size = dl.getTypeAllocSize(pointee_ty);

  llvm::MaybeAlign effective_align;
  if (align.has_value()) {
    effective_align = *align;
  } else if (
      auto* alloca =
          llvm::dyn_cast<llvm::AllocaInst>(dst_ptr->stripPointerCasts())) {
    effective_align = alloca->getAlign();
  } else {
    effective_align = dl.getPrefTypeAlign(pointee_ty);
  }

  builder.CreateMemSet(dst_ptr, builder.getInt8(0), byte_size, effective_align);
}

}  // namespace lyra::lowering::mir_to_llvm
