#include <expected>

#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/Support/Casting.h>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/commit.hpp"
#include "lyra/llvm_backend/commit/access.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/union_storage.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

// Store to design slot with runtime notification.
// Precondition: target.canonical_signal_id must have value.
void StoreDesignWithNotify(
    Context& ctx, llvm::Value* new_value, const WriteTarget& target) {
  if (!target.canonical_signal_id.has_value()) {
    throw common::InternalError(
        "StoreDesignWithNotify", "called with non-design WriteTarget");
  }

  auto& builder = ctx.GetBuilder();
  auto& llvm_ctx = ctx.GetLlvmContext();

  llvm::Type* value_type = new_value->getType();

  // Sanity check: packed stores should be integer or struct types, not pointers
  if (value_type->isPointerTy()) {
    throw common::InternalError(
        "StoreDesignWithNotify",
        "called with pointer type - use container/string commit for managed "
        "types");
  }

  auto byte_size = static_cast<uint32_t>(
      ctx.GetModule().getDataLayout().getTypeAllocSize(value_type));
  auto signal_id = *target.canonical_signal_id;

  // Store new value to a temp alloca (notify helper reads from pointer)
  auto* temp = builder.CreateAlloca(value_type, nullptr, "notify_tmp");
  builder.CreateStore(new_value, temp);

  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);
  builder.CreateCall(
      ctx.GetLyraStorePacked(), {ctx.GetEnginePointer(), target.ptr, temp,
                                 llvm::ConstantInt::get(i32_ty, byte_size),
                                 llvm::ConstantInt::get(i32_ty, signal_id)});
}

}  // namespace

namespace detail {

// Store a plain (non-managed) value to a field pointer.
// For field-by-field assignment - no design-slot notify (fields don't have
// WriteTarget). Future extension point if we need field-level notify for design
// slots with struct fields.
void CommitPlainField(
    Context& ctx, llvm::Value* ptr, llvm::Value* value, TypeId /*type_id*/) {
  // For non-managed fields, just store. type_id reserved for future use
  // (e.g., if we need field-level notify for design slots with struct fields).
  ctx.GetBuilder().CreateStore(value, ptr);
}

}  // namespace detail

namespace {

// Store a packed value to a WriteTarget with design-slot notification.
// If canonical_signal_id has value, notifies runtime. Otherwise plain store.
// Internal to commit module - external callers use CommitPackedValueRaw.
void StorePackedToWriteTarget(
    Context& ctx, llvm::Value* new_value, const WriteTarget& wt) {
  if (wt.canonical_signal_id.has_value()) {
    StoreDesignWithNotify(ctx, new_value, wt);
  } else {
    ctx.GetBuilder().CreateStore(new_value, wt.ptr);
  }
}

}  // namespace

void CommitPackedValueRaw(
    Context& ctx, mir::PlaceId target, llvm::Value* value) {
  auto wt_or_err = commit::Access::GetWriteTarget(ctx, target);
  if (!wt_or_err) {
    throw common::InternalError(
        "CommitPackedValueRaw", "failed to resolve WriteTarget for target");
  }
  StorePackedToWriteTarget(ctx, value, *wt_or_err);
}

namespace {

// 4-state: coerce raw to {val, unk} struct matching storage_type
auto StoreFourStateRaw(
    Context& ctx, const WriteTarget& wt, llvm::Value* raw,
    llvm::StructType* storage_type) -> Result<void> {
  auto& builder = ctx.GetBuilder();
  auto* val_type = storage_type->getElementType(0);
  auto* unk_type = storage_type->getElementType(1);

  llvm::Value* val = nullptr;
  llvm::Value* unk = nullptr;
  if (raw->getType()->isStructTy()) {
    // 4-state source: extract and coerce
    // INVARIANT: struct-shaped raw values in this codebase are always {val,
    // unk} pairs from LowerOperandRaw. No other struct shapes should reach
    // here.
    auto* raw_struct = llvm::cast<llvm::StructType>(raw->getType());
    if (raw_struct->getNumElements() != 2) {
      throw common::InternalError(
          "StoreFourStateRaw",
          "expected {val, unk} struct, got different shape");
    }
    val = builder.CreateExtractValue(raw, 0, "store.val");
    unk = builder.CreateExtractValue(raw, 1, "store.unk");
  } else {
    // 2-state source: wrap with unk=0
    val = raw;
    unk = llvm::ConstantInt::get(unk_type, 0);
  }
  val = builder.CreateZExtOrTrunc(val, val_type, "store.val.fit");
  unk = builder.CreateZExtOrTrunc(unk, unk_type, "store.unk.fit");

  llvm::Value* packed = llvm::UndefValue::get(storage_type);
  packed = builder.CreateInsertValue(packed, val, 0);
  packed = builder.CreateInsertValue(packed, unk, 1);
  StorePackedToWriteTarget(ctx, packed, wt);
  return {};
}

// 2-state: coerce raw to integer matching storage_type
// BYTE-FOR-BYTE MATCH with current AssignTwoState + LowerOperand behavior:
// 1. LowerOperand does (val & ~unk) at SOURCE width
// 2. AssignTwoState does sext/zext to storage_type based on destination
// signedness
auto StoreTwoStateRaw(
    Context& ctx, const WriteTarget& wt, llvm::Value* raw,
    llvm::Type* storage_type, TypeId type_id) -> Result<void> {
  auto& builder = ctx.GetBuilder();
  const auto& types = ctx.GetTypeArena();
  const Type& type = types[type_id];

  llvm::Value* value = raw;
  if (raw->getType()->isStructTy()) {
    // 4-state source → 2-state target: coerce (val & ~unk) at SOURCE width
    // This matches LowerOperand's behavior exactly - no resize before and/not
    auto* v = builder.CreateExtractValue(raw, 0, "coerce.val");
    auto* u = builder.CreateExtractValue(raw, 1, "coerce.unk");
    auto* not_u = builder.CreateNot(u, "coerce.notunk");
    value = builder.CreateAnd(v, not_u, "coerce.known");
    // value is now at source width, will be resized below
  }

  // Width adjustment - same logic for both 2-state source and coerced 4-state
  // This matches AssignTwoState's resize logic exactly
  if (value->getType() != storage_type && value->getType()->isIntegerTy() &&
      storage_type->isIntegerTy()) {
    if (type.Kind() == TypeKind::kIntegral && type.AsIntegral().is_signed) {
      value = builder.CreateSExtOrTrunc(value, storage_type);
    } else {
      value = builder.CreateZExtOrTrunc(value, storage_type);
    }
  }
  StorePackedToWriteTarget(ctx, value, wt);
  return {};
}

}  // namespace

// Packed value store (2-state or 4-state)
auto CommitPackedValue(
    Context& ctx, const WriteTarget& wt, llvm::Value* raw, TypeId type_id)
    -> Result<void> {
  // Compute destination storage type from type_id (authoritative)
  auto storage_type_or_err = BuildLlvmTypeForTypeId(ctx, type_id);
  if (!storage_type_or_err) return std::unexpected(storage_type_or_err.error());
  llvm::Type* storage_type = *storage_type_or_err;

  if (storage_type->isStructTy()) {
    return StoreFourStateRaw(
        ctx, wt, raw, llvm::cast<llvm::StructType>(storage_type));
  }
  return StoreTwoStateRaw(ctx, wt, raw, storage_type, type_id);
}

}  // namespace lyra::lowering::mir_to_llvm
