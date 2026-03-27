#include <format>
#include <vector>

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Type.h>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_queries.hpp"
#include "lyra/llvm_backend/layout/layout.hpp"
#include "lyra/llvm_backend/layout/union_storage.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

auto BuildUnpackedStructTypeNoUnion(
    llvm::LLVMContext& ctx, const UnpackedStructInfo& info,
    const TypeArena& types) -> llvm::Type*;

auto BuildLlvmTypeForTypeIdNoUnion(
    llvm::LLVMContext& ctx, TypeId type_id, const TypeArena& types)
    -> llvm::Type*;

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
      type.Kind() == TypeKind::kQueue ||
      type.Kind() == TypeKind::kAssociativeArray) {
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
    if (IsIntrinsicallyPackedFourState(type, types)) {
      auto* plane_type = GetBackingLlvmType(ctx, bit_width);
      return llvm::StructType::get(ctx, {plane_type, plane_type});
    }
    return GetBackingLlvmType(ctx, bit_width);
  }
  throw common::InternalError(
      "BuildLlvmTypeForTypeIdNoUnion",
      std::format("unsupported type: {}", ToString(type)));
}

}  // namespace

auto BuildLlvmTypeForTypeId(
    llvm::LLVMContext& ctx, TypeId type_id, const TypeArena& types)
    -> llvm::Type* {
  return BuildLlvmTypeForTypeIdNoUnion(ctx, type_id, types);
}

}  // namespace lyra::lowering::mir_to_llvm
