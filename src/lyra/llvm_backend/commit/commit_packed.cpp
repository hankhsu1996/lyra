#include <cstdint>
#include <expected>
#include <variant>

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Type.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/commit.hpp"
#include "lyra/llvm_backend/commit/access.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/layout/storage_contract.hpp"
#include "lyra/llvm_backend/storage_boundary.hpp"
#include "lyra/mir/handle.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

// Resolve storage spec and arena for a type on-the-fly.
struct ResolvedSpec {
  SlotStorageSpec spec;
  StorageSpecArena arena;
};

auto ResolveSpecForType(Context& ctx, TypeId type_id) -> ResolvedSpec {
  auto mode =
      ctx.IsForceTwoState() ? StorageMode::kTwoState : StorageMode::kNormal;
  const auto& dl = ctx.GetModule().getDataLayout();
  TargetStorageAbi target_abi{
      .pointer_byte_size = static_cast<uint32_t>(dl.getPointerSize()),
      .pointer_alignment =
          static_cast<uint32_t>(dl.getPointerABIAlignment(0).value()),
  };
  ResolvedSpec result;
  result.spec = ResolveStorageSpec(
      type_id, ctx.GetTypeArena(), mode, target_abi, result.arena);
  return result;
}

// LLVM integer type for one lane of canonical packed storage.
auto GetLaneIntType(llvm::LLVMContext& ctx, uint32_t bit_width)
    -> llvm::IntegerType* {
  uint32_t storage_bits = GetStorageByteSize(bit_width) * 8;
  return llvm::IntegerType::get(ctx, storage_bits);
}

// Returns true if byte_size is a power of 2 and <= 16 (eligible for inline
// compare+store instead of runtime LyraStorePacked call).
auto IsInlineStoreSize(uint32_t byte_size) -> bool {
  return byte_size > 0 && byte_size <= 16 && (byte_size & (byte_size - 1)) == 0;
}

// Emit inline compare+store+conditional dirty mark for canonical bits.
// Precondition: canonical_bits is an integer in canonical storage form.
//
// Dispatches on DesignStoreMode:
// - kDirect: plain store, no compare, no dirty-mark (init processes)
// - kNotify: compare + store + dirty-mark with engine guaranteed non-null
//
// For kNotify with full-slot dirty marks when the first-dirty bitmap pointer
// is available, emits an inline first-dirty guard that skips the runtime
// call if the slot is already dirty in the current delta.
void EmitInlineStore(
    Context& ctx, llvm::Value* canonical_bits, const WriteTarget& target) {
  auto& builder = ctx.GetBuilder();
  auto& llvm_ctx = ctx.GetLlvmContext();

  if (ctx.GetDesignStoreMode() == DesignStoreMode::kDirectInit) {
    // Init contract: plain store, no compare, no dirty-mark.
    builder.CreateStore(canonical_bits, target.ptr);
    return;
  }

  // Notify contract: compare + store + conditional dirty-mark.
  // Both kNotifySimulation and kNotifyCrossContext require engine_ptr to
  // exist as an LLVM value on Context (even if it may be null at runtime
  // for kNotifyCrossContext).
  if (ctx.GetEnginePointer() == nullptr) {
    throw common::InternalError(
        "EmitInlineStore", "notify store mode requires engine_ptr on Context");
  }

  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);
  auto* i8_ty = llvm::Type::getInt8Ty(llvm_ctx);
  auto* bits_type = canonical_bits->getType();

  auto* old_bits = builder.CreateLoad(bits_type, target.ptr, "old");
  builder.CreateStore(canonical_bits, target.ptr);

  auto* changed = builder.CreateICmpNE(old_bits, canonical_bits, "changed");

  // For kNotifyGuarded (cross-context callables like user functions),
  // engine may be null at runtime. Guard with a branch.
  llvm::Value* should_mark = changed;
  if (ctx.GetDesignStoreMode() == DesignStoreMode::kNotifyCrossContext) {
    auto* engine_not_null = builder.CreateICmpNE(
        ctx.GetEnginePointer(),
        llvm::ConstantPointerNull::get(llvm::PointerType::getUnqual(llvm_ctx)),
        "engine.nonnull");
    should_mark = builder.CreateAnd(changed, engine_not_null, "should_mark");
  }

  auto* fn = builder.GetInsertBlock()->getParent();
  auto* done_bb = llvm::BasicBlock::Create(llvm_ctx, "store_done", fn);

  bool is_full_slot = (target.dirty_off == 0 && target.dirty_size == 0);
  // Hoisted SSA value from process entry. Non-null at the C++ level means
  // a hoisted LLVM value exists; the runtime value may still be null (when
  // the fast path is forbidden or the update set is uninitialized).
  auto* first_dirty_seen_base = ctx.GetFirstDirtySeenPtr();

  if (is_full_slot && first_dirty_seen_base != nullptr) {
    // Inline first-dirty fast path: check the per-delta seen bitmap.
    // Common case (already dirty) skips the runtime call entirely.
    //
    // At runtime, the bitmap pointer is null when the fast path is
    // forbidden in the current execution context (e.g., comb-fixpoint
    // evaluation where repeated writes carry convergence bookkeeping)
    // or when the update set is not yet initialized. Either way, null
    // falls through to the full runtime dirty-mark path.
    auto* null_ptr =
        llvm::ConstantPointerNull::get(llvm::PointerType::getUnqual(llvm_ctx));
    auto* check_bitmap_bb =
        llvm::BasicBlock::Create(llvm_ctx, "check_bitmap", fn);
    auto* check_seen_bb = llvm::BasicBlock::Create(llvm_ctx, "check_seen", fn);
    auto* mark_first_bb = llvm::BasicBlock::Create(llvm_ctx, "mark_first", fn);
    auto* fallback_bb =
        llvm::BasicBlock::Create(llvm_ctx, "mark_dirty_fallback", fn);

    builder.CreateCondBr(should_mark, check_bitmap_bb, done_bb);

    // Runtime null: fast path forbidden or update set uninitialized.
    builder.SetInsertPoint(check_bitmap_bb);
    auto* slot_id = target.canonical_signal_id->Emit(builder);
    auto* bitmap_nonnull =
        builder.CreateICmpNE(first_dirty_seen_base, null_ptr, "bitmap.nonnull");
    builder.CreateCondBr(bitmap_nonnull, check_seen_bb, fallback_bb);

    // Check delta_seen[slot_id] for first-dirty dedup.
    builder.SetInsertPoint(check_seen_bb);
    auto* seen_ptr =
        builder.CreateGEP(i8_ty, first_dirty_seen_base, {slot_id}, "seen_ptr");
    auto* seen = builder.CreateLoad(i8_ty, seen_ptr, "seen");
    auto* already_dirty = builder.CreateICmpNE(
        seen, llvm::ConstantInt::get(i8_ty, 0), "already_dirty");
    builder.CreateCondBr(already_dirty, done_bb, mark_first_bb);

    // First dirty in this delta: call runtime slow path.
    builder.SetInsertPoint(mark_first_bb);
    builder.CreateCall(
        ctx.GetLyraMarkDirtyFirst(), {ctx.GetEnginePointer(), slot_id});
    builder.CreateBr(done_bb);

    // Fallback: bitmap not available, use old runtime path.
    builder.SetInsertPoint(fallback_bb);
    builder.CreateCall(
        ctx.GetLyraMarkDirty(),
        {ctx.GetEnginePointer(), slot_id, llvm::ConstantInt::get(i32_ty, 0),
         llvm::ConstantInt::get(i32_ty, 0)});
    builder.CreateBr(done_bb);
  } else {
    // Fallback: range-dirty or no bitmap available. Call runtime directly.
    auto* dirty_bb = llvm::BasicBlock::Create(llvm_ctx, "mark_dirty", fn);

    builder.CreateCondBr(should_mark, dirty_bb, done_bb);

    builder.SetInsertPoint(dirty_bb);
    builder.CreateCall(
        ctx.GetLyraMarkDirty(),
        {ctx.GetEnginePointer(), target.canonical_signal_id->Emit(builder),
         llvm::ConstantInt::get(i32_ty, target.dirty_off),
         llvm::ConstantInt::get(i32_ty, target.dirty_size)});
    builder.CreateBr(done_bb);
  }

  builder.SetInsertPoint(done_bb);
}

// Store canonical bytes to a design slot via memcpy (no notification).
// Used for init processes and as a subroutine for non-inline stores.
void EmitDirectCanonicalBytesStore(
    Context& ctx, llvm::Value* canonical_buf, const WriteTarget& target,
    uint32_t byte_size) {
  auto& builder = ctx.GetBuilder();
  builder.CreateMemCpy(
      target.ptr, llvm::MaybeAlign(), canonical_buf, llvm::MaybeAlign(),
      byte_size);
}

// Materialize canonical storage bytes and call LyraStorePacked.
// LyraStorePacked consumes canonical bytes by pointer.
// Simulation contract only: engine must be non-null.
void EmitStoreCanonicalBytesCall(
    Context& ctx, llvm::Value* canonical_buf, const WriteTarget& target,
    uint32_t byte_size) {
  auto& builder = ctx.GetBuilder();
  auto* i32_ty = llvm::Type::getInt32Ty(ctx.GetLlvmContext());

  builder.CreateCall(
      ctx.GetLyraStorePacked(),
      {ctx.GetEnginePointer(), target.ptr, canonical_buf,
       llvm::ConstantInt::get(i32_ty, byte_size),
       target.canonical_signal_id->Emit(builder),
       llvm::ConstantInt::get(i32_ty, target.dirty_off),
       llvm::ConstantInt::get(i32_ty, target.dirty_size)});
}

// Store to design slot with mode-appropriate notification.
// All decisions driven by SlotStorageSpec. No LLVM type introspection.
//
// In kDirect mode (init): plain store/memcpy, no compare, no dirty-mark.
// In kNotify mode (simulation): compare + store + conditional dirty-mark.
//
// Precondition: for packed values, new_value must already be at storage
// lane width (caller must have invoked LowerToStorageLaneWidth).
void StoreDesignWithNotify(
    Context& ctx, llvm::Value* new_value, const WriteTarget& target,
    const SlotStorageSpec& spec, const StorageSpecArena& arena) {
  if (!target.canonical_signal_id.has_value()) {
    throw common::InternalError(
        "StoreDesignWithNotify", "called with non-design WriteTarget");
  }

  auto& builder = ctx.GetBuilder();
  uint32_t byte_size = spec.TotalByteSize();
  bool is_direct = ctx.GetDesignStoreMode() == DesignStoreMode::kDirectInit;

  bool is_scalar = std::holds_alternative<PackedStorageSpec>(spec.data) ||
                   std::holds_alternative<FloatStorageSpec>(spec.data);

  if (is_scalar) {
    auto* canonical = EmitPackedToCanonicalBits(builder, new_value, spec);
    if (IsInlineStoreSize(byte_size)) {
      EmitInlineStore(ctx, canonical, target);
    } else {
      // Large scalar: materialize canonical bits into entry-block buffer
      auto* func = builder.GetInsertBlock()->getParent();
      llvm::IRBuilder<> entry_builder(
          &func->getEntryBlock(), func->getEntryBlock().begin());
      auto* temp = entry_builder.CreateAlloca(
          canonical->getType(), nullptr, "canon_buf");
      builder.CreateStore(canonical, temp);
      if (is_direct) {
        EmitDirectCanonicalBytesStore(ctx, temp, target, byte_size);
      } else {
        EmitStoreCanonicalBytesCall(ctx, temp, target, byte_size);
      }
    }
  } else {
    // Aggregate: materialize canonical bytes into entry-block buffer.
    auto* i8_ty = llvm::Type::getInt8Ty(ctx.GetLlvmContext());
    auto* func = builder.GetInsertBlock()->getParent();
    llvm::IRBuilder<> entry_builder(
        &func->getEntryBlock(), func->getEntryBlock().begin());
    auto* buf_ty = llvm::ArrayType::get(i8_ty, byte_size);
    auto* temp = entry_builder.CreateAlloca(buf_ty, nullptr, "canon_buf");
    // Zero-initialize to normalize padding bytes
    builder.CreateMemSet(
        temp, builder.getInt8(0), byte_size, llvm::MaybeAlign());
    EmitStoreToCanonicalStorage(builder, temp, new_value, spec, arena);
    if (is_direct) {
      EmitDirectCanonicalBytesStore(ctx, temp, target, byte_size);
    } else {
      EmitStoreCanonicalBytesCall(ctx, temp, target, byte_size);
    }
  }
}

// Store a packed value to a WriteTarget with design-slot notification.
void StorePackedToWriteTarget(
    Context& ctx, llvm::Value* new_value, const WriteTarget& wt,
    const SlotStorageSpec& spec, const StorageSpecArena& arena) {
  if (wt.canonical_signal_id.has_value()) {
    StoreDesignWithNotify(ctx, new_value, wt, spec, arena);
  } else {
    // Non-design: process-local variable in LLVM struct memory.
    // Direct store is correct (LLVM manages the layout).
    ctx.GetBuilder().CreateStore(new_value, wt.ptr);
  }
}

// 4-state: coerce raw to {val, unk} struct matching PackedStorageSpec.
// Destination form determined by spec. Source form detected from value
// (checking the SSA compute form of a transient value is unavoidable).
auto StoreFourStateRaw(
    Context& ctx, const WriteTarget& wt, llvm::Value* raw,
    const PackedStorageSpec& packed_spec) -> Result<void> {
  auto& builder = ctx.GetBuilder();
  auto* lane_ty = GetLaneIntType(ctx.GetLlvmContext(), packed_spec.bit_width);

  llvm::Value* val = nullptr;
  llvm::Value* unk = nullptr;
  if (raw->getType()->isStructTy()) {
    val = builder.CreateExtractValue(raw, 0, "store.val");
    unk = builder.CreateExtractValue(raw, 1, "store.unk");
  } else {
    val = raw;
    unk = llvm::ConstantInt::get(lane_ty, 0);
  }
  val = builder.CreateZExtOrTrunc(val, lane_ty, "store.val.fit");
  unk = builder.CreateZExtOrTrunc(unk, lane_ty, "store.unk.fit");

  auto* struct_ty =
      llvm::StructType::get(ctx.GetLlvmContext(), {lane_ty, lane_ty});
  llvm::Value* packed = llvm::UndefValue::get(struct_ty);
  packed = builder.CreateInsertValue(packed, val, 0);
  packed = builder.CreateInsertValue(packed, unk, 1);

  SlotStorageSpec spec{.data = packed_spec};
  StorageSpecArena arena;
  StorePackedToWriteTarget(ctx, packed, wt, spec, arena);
  return {};
}

// 2-state: coerce raw to integer matching PackedStorageSpec.
auto StoreTwoStateRaw(
    Context& ctx, const WriteTarget& wt, llvm::Value* raw,
    const PackedStorageSpec& packed_spec, TypeId type_id) -> Result<void> {
  auto& builder = ctx.GetBuilder();
  auto* lane_ty = GetLaneIntType(ctx.GetLlvmContext(), packed_spec.bit_width);
  const auto& types = ctx.GetTypeArena();
  const Type& type = types[type_id];

  llvm::Value* value = raw;
  if (raw->getType()->isStructTy()) {
    // 4-state source -> 2-state target: coerce (val & ~unk) at SOURCE width
    auto* v = builder.CreateExtractValue(raw, 0, "coerce.val");
    auto* u = builder.CreateExtractValue(raw, 1, "coerce.unk");
    auto* not_u = builder.CreateNot(u, "coerce.notunk");
    value = builder.CreateAnd(v, not_u, "coerce.known");
  }

  // Width adjustment using signedness from type
  if (value->getType() != lane_ty) {
    if (type.Kind() == TypeKind::kIntegral && type.AsIntegral().is_signed) {
      value = builder.CreateSExtOrTrunc(value, lane_ty);
    } else {
      value = builder.CreateZExtOrTrunc(value, lane_ty);
    }
  }

  SlotStorageSpec spec{.data = packed_spec};
  StorageSpecArena arena;
  StorePackedToWriteTarget(ctx, value, wt, spec, arena);
  return {};
}

}  // namespace

namespace detail {

void CommitPlainField(Context& ctx, llvm::Value* ptr, llvm::Value* value) {
  ctx.GetBuilder().CreateStore(value, ptr);
}

}  // namespace detail

void CommitPackedValueRaw(
    Context& ctx, mir::PlaceId target, llvm::Value* value, TypeId type_id) {
  auto wt_or_err = commit::Access::GetWriteTarget(ctx, target);
  if (!wt_or_err) {
    throw common::InternalError(
        "CommitPackedValueRaw", "failed to resolve WriteTarget for target");
  }
  auto resolved = ResolveSpecForType(ctx, type_id);

  // Explicit lowering: coerce packed values from semantic width to storage
  // lane width before passing to the storage boundary.
  llvm::Value* lowered = value;
  if (auto* packed = std::get_if<PackedStorageSpec>(&resolved.spec.data)) {
    lowered = LowerToStorageLaneWidth(ctx.GetBuilder(), value, *packed);
  }

  StorePackedToWriteTarget(
      ctx, lowered, *wt_or_err, resolved.spec, resolved.arena);
}

namespace {

// 4-state: coerce raw to {val, unk} struct matching storage spec
// 2-state: coerce raw to integer matching storage spec
// Decision driven by PackedStorageSpec::is_four_state, not LLVM type shape.
auto CommitPackedValueWithSpec(
    Context& ctx, const WriteTarget& wt, llvm::Value* raw,
    const PackedStorageSpec& packed_spec, TypeId type_id) -> Result<void> {
  if (packed_spec.is_four_state) {
    if (ctx.IsForceTwoState()) {
      throw common::InternalError(
          "CommitPackedValue", "four-state storage in two-state mode");
    }
    return StoreFourStateRaw(ctx, wt, raw, packed_spec);
  }
  return StoreTwoStateRaw(ctx, wt, raw, packed_spec, type_id);
}

}  // namespace

auto CommitPackedValue(
    Context& ctx, const WriteTarget& wt, llvm::Value* raw, TypeId type_id)
    -> Result<void> {
  auto resolved = ResolveSpecForType(ctx, type_id);

  if (auto* packed_spec = std::get_if<PackedStorageSpec>(&resolved.spec.data)) {
    return CommitPackedValueWithSpec(ctx, wt, raw, *packed_spec, type_id);
  }

  if (std::holds_alternative<FloatStorageSpec>(resolved.spec.data)) {
    // Float/real: value is already in correct SSA form (float/double).
    // No coercion needed.
    StorePackedToWriteTarget(ctx, raw, wt, resolved.spec, resolved.arena);
    return {};
  }

  throw common::InternalError(
      "CommitPackedValue",
      "expected PackedStorageSpec or FloatStorageSpec for scalar value");
}

auto GetPackedPlanesPtr(Context& ctx, mir::PlaceId target, TypeId type_id)
    -> Result<PackedPlanesPtr> {
  auto wt_or_err = commit::Access::GetWriteTarget(ctx, target);
  if (!wt_or_err) {
    return std::unexpected(wt_or_err.error());
  }
  const auto& wt = *wt_or_err;

  auto resolved = ResolveSpecForType(ctx, type_id);
  auto* packed_spec = std::get_if<PackedStorageSpec>(&resolved.spec.data);
  if (packed_spec == nullptr) {
    throw common::InternalError(
        "GetPackedPlanesPtr", "expected PackedStorageSpec");
  }

  auto& builder = ctx.GetBuilder();

  PackedPlanesPtr result;
  result.root_ptr = wt.ptr;
  result.signal_id = wt.canonical_signal_id;

  if (packed_spec->is_four_state) {
    result.val_ptr = wt.ptr;
    auto* i8_ty = llvm::Type::getInt8Ty(ctx.GetLlvmContext());
    result.unk_ptr = builder.CreateGEP(
        i8_ty, wt.ptr, builder.getInt64(packed_spec->UnknownLaneOffset()),
        "planes.unk");
  } else {
    result.val_ptr = wt.ptr;
    result.unk_ptr = nullptr;
  }

  return result;
}

}  // namespace lyra::lowering::mir_to_llvm
