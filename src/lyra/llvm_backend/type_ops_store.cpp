#include "lyra/llvm_backend/type_ops_store.hpp"

#include <algorithm>
#include <cstdint>
#include <expected>
#include <format>
#include <variant>
#include <vector>

#include <llvm/IR/Constants.h>
#include <llvm/IR/DataLayout.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/type_ops_managed.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
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

// Build LLVM type for an unpacked struct TypeId (LLVMContext-only version).
// Does NOT support unions in struct fields - use Context-aware version instead.
auto BuildUnpackedStructTypeNoUnion(
    llvm::LLVMContext& ctx, const UnpackedStructInfo& info,
    const TypeArena& types) -> llvm::Type*;

// Forward declaration of LLVMContext-only version
auto BuildLlvmTypeForTypeIdNoUnion(
    llvm::LLVMContext& ctx, TypeId type_id, const TypeArena& types)
    -> llvm::Type*;

// Build LLVM type for an unpacked struct with Context (supports unions).
auto BuildUnpackedStructTypeWithContext(
    Context& context, const UnpackedStructInfo& info) -> Result<llvm::Type*> {
  std::vector<llvm::Type*> field_types;
  field_types.reserve(info.fields.size());
  for (const auto& field : info.fields) {
    auto field_type = BuildLlvmTypeForTypeId(context, field.type);
    if (!field_type) return std::unexpected(field_type.error());
    field_types.push_back(*field_type);
  }
  return llvm::StructType::get(context.GetLlvmContext(), field_types);
}

auto BuildUnpackedStructTypeNoUnion(
    llvm::LLVMContext& ctx, const UnpackedStructInfo& info,
    const TypeArena& types) -> llvm::Type* {
  std::vector<llvm::Type*> field_types;
  field_types.reserve(info.fields.size());
  for (const auto& field : info.fields) {
    field_types.push_back(
        BuildLlvmTypeForTypeIdNoUnion(ctx, field.type, types));
  }
  return llvm::StructType::get(ctx, field_types);
}

auto BuildLlvmTypeForTypeIdNoUnion(
    llvm::LLVMContext& ctx, TypeId type_id, const TypeArena& types)
    -> llvm::Type* {
  const Type& type = types[type_id];

  if (type.Kind() == TypeKind::kString) {
    return llvm::PointerType::getUnqual(ctx);
  }
  if (type.Kind() == TypeKind::kUnpackedStruct) {
    return BuildUnpackedStructTypeNoUnion(ctx, type.AsUnpackedStruct(), types);
  }
  if (type.Kind() == TypeKind::kUnpackedUnion) {
    throw common::InternalError(
        "BuildLlvmTypeForTypeId",
        "union types require Context for correct DataLayout-based sizing; "
        "use BuildLlvmTypeForTypeId(Context&, TypeId) instead");
  }
  if (type.Kind() == TypeKind::kUnpackedArray) {
    const auto& info = type.AsUnpackedArray();
    llvm::Type* elem =
        BuildLlvmTypeForTypeIdNoUnion(ctx, info.element_type, types);
    return llvm::ArrayType::get(elem, info.range.Size());
  }
  if (type.Kind() == TypeKind::kDynamicArray ||
      type.Kind() == TypeKind::kQueue) {
    return llvm::PointerType::getUnqual(ctx);
  }
  if (type.Kind() == TypeKind::kReal) {
    return llvm::Type::getDoubleTy(ctx);
  }
  if (type.Kind() == TypeKind::kShortReal) {
    return llvm::Type::getFloatTy(ctx);
  }
  if (IsPacked(type)) {
    auto bit_width = PackedBitWidth(type, types);
    if (IsPackedFourState(type, types)) {
      auto* plane_type = GetLlvmStorageType(ctx, bit_width);
      return llvm::StructType::get(ctx, {plane_type, plane_type});
    }
    return GetLlvmStorageType(ctx, bit_width);
  }
  throw common::InternalError(
      "BuildLlvmTypeForTypeIdNoUnion",
      std::format("unsupported type: {}", ToString(type)));
}

}  // namespace

auto BuildLlvmTypeForTypeId(Context& context, TypeId type_id)
    -> Result<llvm::Type*> {
  const auto& types = context.GetTypeArena();
  const Type& type = types[type_id];

  if (type.Kind() == TypeKind::kString) {
    return llvm::PointerType::getUnqual(context.GetLlvmContext());
  }
  if (type.Kind() == TypeKind::kUnpackedStruct) {
    return BuildUnpackedStructTypeWithContext(context, type.AsUnpackedStruct());
  }
  if (type.Kind() == TypeKind::kUnpackedUnion) {
    // Use GetUnionStorageInfo which is the single source of truth for union
    // layout, backed by DataLayout
    auto result = GetUnionStorageInfo(context, type_id);
    if (!result) return std::unexpected(result.error());
    return result->storage_type;
  }
  if (type.Kind() == TypeKind::kUnpackedArray) {
    const auto& info = type.AsUnpackedArray();
    auto elem = BuildLlvmTypeForTypeId(context, info.element_type);
    if (!elem) return std::unexpected(elem.error());
    return llvm::ArrayType::get(*elem, info.range.Size());
  }
  if (type.Kind() == TypeKind::kDynamicArray ||
      type.Kind() == TypeKind::kQueue) {
    return llvm::PointerType::getUnqual(context.GetLlvmContext());
  }
  if (type.Kind() == TypeKind::kReal) {
    return llvm::Type::getDoubleTy(context.GetLlvmContext());
  }
  if (type.Kind() == TypeKind::kShortReal) {
    return llvm::Type::getFloatTy(context.GetLlvmContext());
  }
  if (IsPacked(type)) {
    auto bit_width = PackedBitWidth(type, types);
    if (IsPackedFourState(type, types)) {
      auto* plane_type =
          GetLlvmStorageType(context.GetLlvmContext(), bit_width);
      return llvm::StructType::get(
          context.GetLlvmContext(), {plane_type, plane_type});
    }
    return GetLlvmStorageType(context.GetLlvmContext(), bit_width);
  }
  throw common::InternalError(
      "BuildLlvmTypeForTypeId",
      std::format("unsupported type: {}", ToString(type)));
}

auto BuildLlvmTypeForTypeId(
    llvm::LLVMContext& ctx, TypeId type_id, const TypeArena& types)
    -> llvm::Type* {
  // Delegate to the NoUnion version which throws for unions.
  // Use the Context-aware overload when unions may be present.
  return BuildLlvmTypeForTypeIdNoUnion(ctx, type_id, types);
}

void StoreToWriteTarget(
    Context& context, llvm::Value* new_value, const WriteTarget& target) {
  if (target.canonical_signal_id.has_value()) {
    StoreDesignWithNotify(context, new_value, target);
  } else {
    context.GetBuilder().CreateStore(new_value, target.ptr);
  }
}

void StoreDesignWithNotify(
    Context& context, llvm::Value* new_value, const WriteTarget& target) {
  if (!target.canonical_signal_id.has_value()) {
    throw common::InternalError(
        "StoreDesignWithNotify", "called with non-design WriteTarget");
  }

  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();

  // INVARIANT: new_value->getType() must match the storage type of the
  // destination slot. Callers are responsible for coercing the value to the
  // correct type (zext/trunc for width, insert/extract for 4-state packing)
  // before calling this function.
  llvm::Type* value_type = new_value->getType();

  // Sanity check: packed stores should be integer or struct types, not pointers
  // (pointer types use StoreDynArrayToWriteTarget or StoreStringToWriteTarget)
  if (value_type->isPointerTy()) {
    throw common::InternalError(
        "StoreDesignWithNotify",
        "called with pointer type - use StoreDynArrayToWriteTarget or "
        "StoreStringToWriteTarget for managed types");
  }

  auto byte_size = static_cast<uint32_t>(
      context.GetModule().getDataLayout().getTypeAllocSize(value_type));
  auto signal_id = *target.canonical_signal_id;

  // Store new value to a temp alloca (notify helper reads from pointer)
  auto* temp = builder.CreateAlloca(value_type, nullptr, "notify_tmp");
  builder.CreateStore(new_value, temp);

  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);
  builder.CreateCall(
      context.GetLyraStorePacked(),
      {context.GetEnginePointer(), target.ptr, temp,
       llvm::ConstantInt::get(i32_ty, byte_size),
       llvm::ConstantInt::get(i32_ty, signal_id)});
}

void StoreDynArrayToWriteTarget(
    Context& context, llvm::Value* new_handle, const WriteTarget& target,
    TypeId type_id) {
  auto& builder = context.GetBuilder();
  auto* ptr_ty = llvm::PointerType::getUnqual(context.GetLlvmContext());

  if (target.canonical_signal_id.has_value()) {
    // Design slot: load old first, then atomic store+notify, then release old
    auto* old_handle = builder.CreateLoad(ptr_ty, target.ptr, "da.old");
    auto* i32_ty = llvm::Type::getInt32Ty(context.GetLlvmContext());
    builder.CreateCall(
        context.GetLyraStoreDynArray(),
        {context.GetEnginePointer(), target.ptr, new_handle,
         llvm::ConstantInt::get(i32_ty, *target.canonical_signal_id)});
    builder.CreateCall(context.GetLyraDynArrayRelease(), {old_handle});
  } else {
    // Non-design: destroy old, store new
    Destroy(context, target.ptr, type_id);
    builder.CreateStore(new_handle, target.ptr);
  }
}

void StoreStringToWriteTarget(
    Context& context, llvm::Value* new_val, const WriteTarget& target,
    TypeId type_id) {
  auto& builder = context.GetBuilder();
  auto* ptr_ty = llvm::PointerType::getUnqual(context.GetLlvmContext());

  if (target.canonical_signal_id.has_value()) {
    // Design slot: load old first, then atomic store+notify, then release old
    auto* old_val = builder.CreateLoad(ptr_ty, target.ptr, "str.old");
    auto* i32_ty = llvm::Type::getInt32Ty(context.GetLlvmContext());
    builder.CreateCall(
        context.GetLyraStoreString(),
        {context.GetEnginePointer(), target.ptr, new_val,
         llvm::ConstantInt::get(i32_ty, *target.canonical_signal_id)});
    builder.CreateCall(context.GetLyraStringRelease(), {old_val});
  } else {
    // Non-design: destroy old, store new
    Destroy(context, target.ptr, type_id);
    builder.CreateStore(new_val, target.ptr);
  }
}

void StoreStringFieldRaw(
    Context& context, llvm::Value* target_ptr, llvm::Value* handle,
    OwnershipPolicy policy, TypeId type_id) {
  auto& builder = context.GetBuilder();

  // Apply ownership policy
  if (policy == OwnershipPolicy::kClone) {
    handle = builder.CreateCall(context.GetLyraStringRetain(), {handle});
  }
  // kMove: handle already has ownership, no retain needed

  // Destroy old value and store new
  Destroy(context, target_ptr, type_id);
  builder.CreateStore(handle, target_ptr);
}

void StorePlainFieldRaw(
    Context& context, llvm::Value* target_ptr, llvm::Value* value,
    TypeId /*type_id*/) {
  // For non-managed fields, just store. type_id reserved for future use
  // (e.g., if we need field-level notify for design slots with struct fields).
  context.GetBuilder().CreateStore(value, target_ptr);
}

void NotifyUnionStore(
    Context& context, const WriteTarget& target, uint32_t size) {
  if (!target.canonical_signal_id.has_value()) {
    return;  // Not a design slot, nothing to notify
  }

  auto& builder = context.GetBuilder();
  auto* i32_ty = llvm::Type::getInt32Ty(context.GetLlvmContext());
  builder.CreateCall(
      context.GetLyraStorePacked(),
      {context.GetEnginePointer(), target.ptr,
       target.ptr,  // For unions, source = target after memcpy
       llvm::ConstantInt::get(i32_ty, size),
       llvm::ConstantInt::get(i32_ty, *target.canonical_signal_id)});
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

  // Type should already be validated/cached during lowering. If this fails,
  // it indicates a bug where we're trying to destroy a type that was never
  // properly created.
  auto llvm_struct_type_result =
      BuildLlvmTypeForTypeId(context, struct_type_id);
  if (!llvm_struct_type_result) {
    throw common::InternalError(
        "DestroyStructFields",
        "failed to get LLVM type for struct - type was not properly lowered");
  }
  llvm::Type* llvm_struct_type = *llvm_struct_type_result;

  for (size_t i = 0; i < struct_info.fields.size(); ++i) {
    const auto& field = struct_info.fields[i];
    auto field_idx = static_cast<unsigned>(i);
    llvm::Value* field_ptr =
        builder.CreateStructGEP(llvm_struct_type, ptr, field_idx);
    Destroy(context, field_ptr, field.type);
  }
}

namespace {

// Get the ABI alignment for a member type in bytes
auto GetMemberAlignment(
    Context& context, TypeId member_type, const TypeArena& types)
    -> Result<uint32_t> {
  const auto& dl = context.GetModule().getDataLayout();
  const Type& type = types[member_type];

  switch (type.Kind()) {
    case TypeKind::kIntegral: {
      uint32_t bit_width = type.AsIntegral().bit_width;
      llvm::Type* llvm_type =
          GetLlvmStorageType(context.GetLlvmContext(), bit_width);
      return dl.getABITypeAlign(llvm_type).value();
    }
    case TypeKind::kReal:
      return dl
          .getABITypeAlign(llvm::Type::getDoubleTy(context.GetLlvmContext()))
          .value();
    case TypeKind::kShortReal:
      return dl
          .getABITypeAlign(llvm::Type::getFloatTy(context.GetLlvmContext()))
          .value();
    case TypeKind::kPackedArray:
    case TypeKind::kPackedStruct:
    case TypeKind::kEnum: {
      uint32_t bit_width = PackedBitWidth(type, types);
      llvm::Type* llvm_type =
          GetLlvmStorageType(context.GetLlvmContext(), bit_width);
      return dl.getABITypeAlign(llvm_type).value();
    }
    case TypeKind::kUnpackedStruct: {
      auto llvm_type = BuildLlvmTypeForTypeId(context, member_type);
      if (!llvm_type) return std::unexpected(llvm_type.error());
      return dl.getABITypeAlign(*llvm_type).value();
    }
    case TypeKind::kUnpackedArray: {
      const auto& info = type.AsUnpackedArray();
      return GetMemberAlignment(context, info.element_type, types);
    }
    case TypeKind::kUnpackedUnion: {
      // Recursive: get the cached or compute alignment
      const auto* cached = context.GetCachedUnionStorageInfo(member_type);
      if (cached != nullptr) {
        return cached->align;
      }
      // Compute without caching (rare case of nested unions)
      const auto& union_info = type.AsUnpackedUnion();
      uint32_t max_align = 1;
      for (const auto& m : union_info.members) {
        auto align = GetMemberAlignment(context, m.type, types);
        if (!align) return std::unexpected(align.error());
        max_align = std::max(max_align, *align);
      }
      return max_align;
    }
    default:
      return 1;  // Default alignment
  }
}

// Get the allocation size for a member type in bytes
auto GetMemberAllocSize(
    Context& context, TypeId member_type, const TypeArena& types)
    -> Result<uint32_t> {
  const auto& dl = context.GetModule().getDataLayout();
  const Type& type = types[member_type];

  switch (type.Kind()) {
    case TypeKind::kIntegral: {
      uint32_t bit_width = type.AsIntegral().bit_width;
      llvm::Type* llvm_type =
          GetLlvmStorageType(context.GetLlvmContext(), bit_width);
      return static_cast<uint32_t>(dl.getTypeAllocSize(llvm_type));
    }
    case TypeKind::kReal:
      return static_cast<uint32_t>(dl.getTypeAllocSize(
          llvm::Type::getDoubleTy(context.GetLlvmContext())));
    case TypeKind::kShortReal:
      return static_cast<uint32_t>(dl.getTypeAllocSize(
          llvm::Type::getFloatTy(context.GetLlvmContext())));
    case TypeKind::kPackedArray:
    case TypeKind::kPackedStruct:
    case TypeKind::kEnum: {
      uint32_t bit_width = PackedBitWidth(type, types);
      llvm::Type* llvm_type =
          GetLlvmStorageType(context.GetLlvmContext(), bit_width);
      return static_cast<uint32_t>(dl.getTypeAllocSize(llvm_type));
    }
    case TypeKind::kUnpackedStruct: {
      auto llvm_type = BuildLlvmTypeForTypeId(context, member_type);
      if (!llvm_type) return std::unexpected(llvm_type.error());
      return static_cast<uint32_t>(dl.getTypeAllocSize(*llvm_type));
    }
    case TypeKind::kUnpackedArray: {
      auto llvm_type = BuildLlvmTypeForTypeId(context, member_type);
      if (!llvm_type) return std::unexpected(llvm_type.error());
      return static_cast<uint32_t>(dl.getTypeAllocSize(*llvm_type));
    }
    case TypeKind::kUnpackedUnion: {
      const auto* cached = context.GetCachedUnionStorageInfo(member_type);
      if (cached != nullptr) {
        return cached->size;
      }
      // Compute without caching
      const auto& union_info = type.AsUnpackedUnion();
      uint32_t max_size = 0;
      for (const auto& m : union_info.members) {
        auto size = GetMemberAllocSize(context, m.type, types);
        if (!size) return std::unexpected(size.error());
        max_size = std::max(max_size, *size);
      }
      uint32_t max_align = 1;
      for (const auto& m : union_info.members) {
        auto align = GetMemberAlignment(context, m.type, types);
        if (!align) return std::unexpected(align.error());
        max_align = std::max(max_align, *align);
      }
      // Round up to alignment
      return (max_size + max_align - 1) / max_align * max_align;
    }
    default:
      throw common::InternalError(
          "GetMemberAllocSize",
          std::format("unsupported type kind: {}", ToString(type.Kind())));
  }
}

}  // namespace

auto GetUnionStorageInfo(Context& context, TypeId union_type_id)
    -> Result<UnionStorageInfo> {
  // Check cache first
  const auto* cached = context.GetCachedUnionStorageInfo(union_type_id);
  if (cached != nullptr) {
    return UnionStorageInfo{
        .size = cached->size,
        .align = cached->align,
        .storage_type = cached->storage_type,
    };
  }

  const auto& types = context.GetTypeArena();
  const Type& type = types[union_type_id];
  const auto& union_info = type.AsUnpackedUnion();

  // Validate: reject 4-state unions (LLVM backend limitation)
  if (union_info.storage_is_four_state) {
    return std::unexpected(context.GetDiagnosticContext().MakeUnsupported(
        context.GetCurrentOrigin(),
        "unions with 4-state members are not yet supported in LLVM backend",
        UnsupportedCategory::kType));
  }

  // Compute max allocation size and alignment across all members
  uint32_t max_size = 0;
  uint32_t max_align = 1;
  for (const auto& member : union_info.members) {
    auto member_size = GetMemberAllocSize(context, member.type, types);
    if (!member_size) return std::unexpected(member_size.error());
    auto member_align = GetMemberAlignment(context, member.type, types);
    if (!member_align) return std::unexpected(member_align.error());
    max_size = std::max(max_size, *member_size);
    max_align = std::max(max_align, *member_align);
  }

  // Round size up to alignment
  uint32_t aligned_size = (max_size + max_align - 1) / max_align * max_align;

  // Create [size x i8] storage type
  auto* i8_ty = llvm::Type::getInt8Ty(context.GetLlvmContext());
  auto* storage_type = llvm::ArrayType::get(i8_ty, aligned_size);

  // Cache and return
  Context::CachedUnionInfo info{
      .size = aligned_size,
      .align = max_align,
      .storage_type = storage_type,
  };
  context.GetOrCreateUnionStorageInfo(union_type_id, info);

  return UnionStorageInfo{
      .size = aligned_size,
      .align = max_align,
      .storage_type = storage_type,
  };
}

auto BuildUnpackedUnionType(
    Context& context, TypeId union_type_id, const TypeArena& /*types*/)
    -> Result<llvm::Type*> {
  auto result = GetUnionStorageInfo(context, union_type_id);
  if (!result) return std::unexpected(result.error());
  return result->storage_type;
}

namespace {

// String: retain if clone, store, destroy old
auto StoreStringRaw(
    Context& ctx, const WriteTarget& wt, llvm::Value* handle,
    OwnershipPolicy policy, TypeId type_id) -> Result<void> {
  if (policy == OwnershipPolicy::kClone) {
    handle = ctx.GetBuilder().CreateCall(ctx.GetLyraStringRetain(), {handle});
  }
  // kMove: handle already has ownership, no retain needed
  StoreStringToWriteTarget(ctx, handle, wt, type_id);
  return {};
}

// Container (DynArray, Queue): clone if clone, store, destroy old
auto StoreContainerRaw(
    Context& ctx, const WriteTarget& wt, llvm::Value* handle,
    OwnershipPolicy policy, TypeId type_id) -> Result<void> {
  const auto& types = ctx.GetTypeArena();
  const Type& type = types[type_id];

  // HARD REQUIREMENT: Only kDynamicArray and kQueue use this path.
  // If ManagedKind::kContainer expands to other types, they need their own
  // path.
  if (type.Kind() != TypeKind::kDynamicArray &&
      type.Kind() != TypeKind::kQueue) {
    throw common::InternalError(
        "StoreContainerRaw", "called with non-container type");
  }

  if (policy == OwnershipPolicy::kClone) {
    handle = ctx.GetBuilder().CreateCall(ctx.GetLyraDynArrayClone(), {handle});
  }
  // kMove: handle already has ownership, no clone needed
  StoreDynArrayToWriteTarget(ctx, handle, wt, type_id);
  return {};
}

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
  StoreToWriteTarget(ctx, packed, wt);
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
  StoreToWriteTarget(ctx, value, wt);
  return {};
}

}  // namespace

auto StoreRawToTarget(
    Context& context, const WriteTarget& target, llvm::Value* raw_value,
    TypeId type_id, OwnershipPolicy policy) -> Result<void> {
  const auto& types = context.GetTypeArena();
  const Type& type = types[type_id];

  // Managed types: ownership matters
  switch (GetManagedKind(type.Kind())) {
    case ManagedKind::kString:
      return StoreStringRaw(context, target, raw_value, policy, type_id);
    case ManagedKind::kContainer:
      return StoreContainerRaw(context, target, raw_value, policy, type_id);
    case ManagedKind::kNone:
      break;
  }

  // Non-managed types: ownership is no-op (kClone == kMove == plain store)
  // Compute destination storage type from type_id (authoritative)
  auto storage_type_or_err = BuildLlvmTypeForTypeId(context, type_id);
  if (!storage_type_or_err) return std::unexpected(storage_type_or_err.error());
  llvm::Type* storage_type = *storage_type_or_err;

  if (storage_type->isStructTy()) {
    return StoreFourStateRaw(
        context, target, raw_value, llvm::cast<llvm::StructType>(storage_type));
  }
  return StoreTwoStateRaw(context, target, raw_value, storage_type, type_id);
}

void NullOutSourceIfMoveTemp(
    Context& context, const mir::Operand& source, OwnershipPolicy policy,
    TypeId type_id) {
  const auto& types = context.GetTypeArena();
  const Type& type = types[type_id];

  // HARD REQUIREMENT: Only callable for managed types.
  if (GetManagedKind(type.Kind()) == ManagedKind::kNone) {
    throw common::InternalError(
        "NullOutSourceIfMoveTemp", "called with non-managed type");
  }

  // Conditions for null-out:
  // 1. Must be a move operation
  if (policy != OwnershipPolicy::kMove) {
    return;
  }

  // 2. Source must be a PlaceId (not a Const)
  if (!std::holds_alternative<mir::PlaceId>(source.payload)) {
    return;
  }

  auto src_place_id = std::get<mir::PlaceId>(source.payload);
  const auto& arena = context.GetMirArena();
  const auto& src_place = arena[src_place_id];

  // 3. Source place root must be kTemp
  if (src_place.root.kind != mir::PlaceRoot::Kind::kTemp) {
    return;
  }

  // Null-out the source handle
  auto src_ptr_result = context.GetPlacePointer(src_place_id);
  if (!src_ptr_result) {
    // If we can't get the pointer, it's an internal error (should never happen)
    throw common::InternalError(
        "NullOutSourceIfMoveTemp", "failed to get source place pointer");
  }

  auto* ptr_ty = llvm::PointerType::getUnqual(context.GetLlvmContext());
  auto* null_val =
      llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptr_ty));
  context.GetBuilder().CreateStore(null_val, *src_ptr_result);
}

}  // namespace lyra::lowering::mir_to_llvm
