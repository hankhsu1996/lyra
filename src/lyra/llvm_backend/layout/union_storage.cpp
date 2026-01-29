#include "lyra/llvm_backend/layout/union_storage.hpp"

#include <algorithm>
#include <cstdint>
#include <expected>
#include <format>
#include <vector>

#include <llvm/IR/DataLayout.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Type.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/lowering/diagnostic_context.hpp"

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

}  // namespace lyra::lowering::mir_to_llvm
