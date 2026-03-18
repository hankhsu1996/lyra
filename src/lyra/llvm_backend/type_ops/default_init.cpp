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
#include "lyra/common/type_utils.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/layout/union_storage.hpp"
#include "lyra/llvm_backend/storage_boundary.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

// Threshold for unrolling small 4-state array init.
// Arrays with count <= this emit direct stores; larger arrays use a loop.
// Chosen to balance IR size vs loop overhead. At 8 elements, unrolling costs
// ~24 IR lines (3 per element) vs ~37 for a loop, and avoids 4 basic blocks.
constexpr uint32_t kSmallArrayUnrollThreshold = 8;

// Delegates to shared canonical storage helper.
void EmitStoreFourStateX(Context& ctx, llvm::Value* ptr, uint32_t bit_width) {
  EmitStoreFourStateXToCanonical(ctx.GetBuilder(), ptr, bit_width);
}

// Local helper that forwards to the public EmitMemsetZero.
void EmitMemsetZeroLocal(
    Context& ctx, llvm::Value* ptr, llvm::Type* llvm_type) {
  EmitMemsetZero(ctx, ptr, llvm_type);
}

// Internal implementation with already_zeroed flag.
// When already_zeroed=true, skips redundant zeroing for 2-state types.
void EmitSVDefaultInitImpl(
    Context& ctx, llvm::Value* ptr, TypeId type_id, bool already_zeroed) {
  auto& builder = ctx.GetBuilder();
  auto& llvm_ctx = ctx.GetLlvmContext();
  const auto& types = ctx.GetTypeArena();
  const Type& type = types[type_id];

  switch (type.Kind()) {
    case TypeKind::kIntegral: {
      uint32_t bit_width = type.AsIntegral().bit_width;
      if (ctx.IsPackedFourState(type)) {
        if (already_zeroed) {
          EmitStoreUnknownMaskToCanonical(builder, ptr, bit_width);
        } else {
          EmitStoreFourStateX(ctx, ptr, bit_width);
        }
      } else {
        // 2-state: store zero (skip if already zeroed)
        if (!already_zeroed) {
          auto* llvm_type = GetLlvmStorageType(llvm_ctx, bit_width);
          auto* zero = llvm::Constant::getNullValue(llvm_type);
          builder.CreateStore(zero, ptr);
        }
      }
      return;
    }

    case TypeKind::kReal: {
      // Real defaults to 0.0. Skip store if already zeroed since IEEE754
      // double 0.0 is all-zero bits (LLVM uses IEEE754 representation).
      if (!already_zeroed) {
        auto* llvm_type = llvm::Type::getDoubleTy(llvm_ctx);
        auto* zero = llvm::ConstantFP::get(llvm_type, 0.0);
        builder.CreateStore(zero, ptr);
      }
      return;
    }

    case TypeKind::kShortReal: {
      // ShortReal defaults to 0.0f. Skip store if already zeroed since IEEE754
      // float 0.0f is all-zero bits (LLVM uses IEEE754 representation).
      if (!already_zeroed) {
        auto* llvm_type = llvm::Type::getFloatTy(llvm_ctx);
        auto* zero = llvm::ConstantFP::get(llvm_type, 0.0);
        builder.CreateStore(zero, ptr);
      }
      return;
    }

    case TypeKind::kString:
    case TypeKind::kDynamicArray:
    case TypeKind::kQueue:
    case TypeKind::kAssociativeArray: {
      // Pointer defaults to null. Skip store if already zeroed since LLVM
      // null pointer is all-zero bits on all supported targets.
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
      if (ctx.IsPackedFourState(type)) {
        // 4-state packed: write X encoding using canonical storage layout.
        if (already_zeroed) {
          EmitStoreUnknownMaskToCanonical(builder, ptr, width);
        } else {
          EmitStoreFourStateX(ctx, ptr, width);
        }
      } else {
        // 2-state packed: store zero (skip if already zeroed)
        if (!already_zeroed) {
          auto* llvm_type = GetLlvmStorageType(llvm_ctx, width);
          auto* zero = llvm::Constant::getNullValue(llvm_type);
          builder.CreateStore(zero, ptr);
        }
      }
      return;
    }

    case TypeKind::kUnpackedStruct: {
      auto llvm_type_result = BuildLlvmTypeForTypeId(ctx, type_id);
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
            ctx, field_ptr, info.fields[i].type, already_zeroed);
      }
      return;
    }

    case TypeKind::kUnpackedArray: {
      auto llvm_type_result = BuildLlvmTypeForTypeId(ctx, type_id);
      if (!llvm_type_result) {
        throw common::InternalError(
            "EmitSVDefaultInitImpl", "failed to build LLVM type for array");
      }
      auto* array_llvm_type = llvm::cast<llvm::ArrayType>(*llvm_type_result);
      const auto& info = type.AsUnpackedArray();
      uint32_t count = info.range.Size();

      if (count == 0) return;  // Empty array - nothing to do

      // Check if this array contains 4-state elements that need X encoding.
      // If not, memset(0) is sufficient and we avoid creating new basic blocks.
      if (!ctx.IsFourState(info.element_type)) {
        if (!already_zeroed) {
          EmitMemsetZeroLocal(ctx, ptr, array_llvm_type);
        }
        return;
      }

      // INVARIANT: ptr points to storage with layout matching array_llvm_type.
      // Callers (local_slot lowering, struct field init) compute ptr via GEP
      // using the same type hierarchy, ensuring consistency with opaque
      // pointers.

      // For 4-state arrays, we need to set X encoding per element.
      // First memset to zero (handles value plane) unless already zeroed.
      if (!already_zeroed) {
        EmitMemsetZeroLocal(ctx, ptr, array_llvm_type);
      }

      // Get element type info for init
      const Type& elem_type = types[info.element_type];

      // For small arrays, unroll directly (no loop, no extra basic blocks).
      // This avoids 4 basic blocks + phi + branch overhead for tiny arrays.
      if (count <= kSmallArrayUnrollThreshold) {
        for (uint32_t i = 0; i < count; ++i) {
          auto* elem_ptr =
              builder.CreateConstGEP2_32(array_llvm_type, ptr, 0, i);

          if (elem_type.Kind() == TypeKind::kIntegral &&
              ctx.IsPackedFourState(elem_type)) {
            uint32_t bit_width = elem_type.AsIntegral().bit_width;
            EmitStoreUnknownMaskToCanonical(builder, elem_ptr, bit_width);
          } else {
            // Nested type containing 4-state
            EmitSVDefaultInitImpl(ctx, elem_ptr, info.element_type, true);
          }
        }
        return;
      }

      // Large arrays: use a loop to avoid IR explosion.
      // Build canonical loop: preheader -> header -> body -> latch ->
      // header/exit
      auto* func = builder.GetInsertBlock()->getParent();
      auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);

      auto* preheader = builder.GetInsertBlock();
      auto* header = llvm::BasicBlock::Create(llvm_ctx, "arr.init.hdr", func);
      auto* body = llvm::BasicBlock::Create(llvm_ctx, "arr.init.body", func);
      auto* latch = llvm::BasicBlock::Create(llvm_ctx, "arr.init.latch", func);
      auto* exit_bb = llvm::BasicBlock::Create(llvm_ctx, "arr.init.exit", func);

      // Preheader -> header
      builder.CreateBr(header);

      // Header: phi + condbr to body/exit
      builder.SetInsertPoint(header);
      auto* phi = builder.CreatePHI(i32_ty, 2, "arr.init.idx");
      phi->addIncoming(llvm::ConstantInt::get(i32_ty, 0), preheader);
      auto* cond = builder.CreateICmpULT(
          phi, llvm::ConstantInt::get(i32_ty, count), "arr.init.cmp");
      builder.CreateCondBr(cond, body, exit_bb);

      // Body: compute elem_ptr and set unknown plane
      builder.SetInsertPoint(body);
      auto* elem_ptr = builder.CreateGEP(
          array_llvm_type, ptr, {builder.getInt32(0), phi}, "arr.init.elem");

      if (elem_type.Kind() == TypeKind::kIntegral &&
          ctx.IsPackedFourState(elem_type)) {
        uint32_t bit_width = elem_type.AsIntegral().bit_width;
        EmitStoreUnknownMaskToCanonical(builder, elem_ptr, bit_width);
      } else {
        // Nested type containing 4-state (struct/array with 4-state fields).
        // Recursive call with already_zeroed=true since we already memset'd
        // (or the caller did).
        EmitSVDefaultInitImpl(ctx, elem_ptr, info.element_type, true);
      }
      builder.CreateBr(latch);

      // Latch: increment index, add phi incoming, branch to header with
      // metadata
      builder.SetInsertPoint(latch);
      auto* next_idx = builder.CreateAdd(
          phi, llvm::ConstantInt::get(i32_ty, 1), "arr.init.next");
      phi->addIncoming(next_idx, latch);

      // Build loop metadata to disable unrolling
      // Pattern: !{!self, !{!"llvm.loop.unroll.disable"}}
      auto* loop_id = llvm::MDNode::getDistinct(llvm_ctx, {nullptr});
      loop_id->replaceOperandWith(0, loop_id);  // Self-reference
      auto* unroll_disable = llvm::MDNode::get(
          llvm_ctx,
          {llvm::MDString::get(llvm_ctx, "llvm.loop.unroll.disable")});
      auto* loop_md = llvm::MDNode::get(llvm_ctx, {loop_id, unroll_disable});
      auto* backedge = builder.CreateBr(header);
      backedge->setMetadata(llvm::LLVMContext::MD_loop, loop_md);

      // Exit: continue with subsequent code
      builder.SetInsertPoint(exit_bb);
      return;
    }

    case TypeKind::kUnpackedUnion: {
      // TODO(hankhsu): 4-state unions not yet supported. Zero-fill for now.
      if (!already_zeroed) {
        auto llvm_type_result = BuildLlvmTypeForTypeId(ctx, type_id);
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

void EmitSVDefaultInit(Context& ctx, llvm::Value* ptr, TypeId type_id) {
  EmitSVDefaultInitImpl(ctx, ptr, type_id, false);
}

void EmitSVDefaultInitAfterZero(
    Context& ctx, llvm::Value* ptr, TypeId type_id) {
  EmitSVDefaultInitImpl(ctx, ptr, type_id, true);
}

void EmitMemsetZero(
    Context& ctx, llvm::Value* dst_ptr, llvm::Type* pointee_ty,
    std::optional<llvm::Align> align) {
  auto& builder = ctx.GetBuilder();
  auto& module = ctx.GetModule();
  const llvm::DataLayout& dl = module.getDataLayout();

  uint64_t byte_size = dl.getTypeAllocSize(pointee_ty);

  // Determine alignment: use provided, infer from alloca, or use preferred
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
