#include <cstddef>
#include <variant>

#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/Support/Casting.h"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/unsupported_error.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/operand.hpp"
#include "lyra/llvm_backend/type_ops_handlers.hpp"
#include "lyra/llvm_backend/type_ops_managed.hpp"
#include "lyra/llvm_backend/type_ops_store.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

// Forward declaration for mutual recursion
void AssignStructFieldByField(
    Context& context, llvm::Value* source_ptr, llvm::Value* target_ptr,
    TypeId struct_type_id, OwnershipPolicy policy);

// Assign a single field, handling string ref counting and nested structs.
// Uses Destroy(target) first, then stores new value.
void AssignField(
    Context& context, llvm::Value* source_field_ptr,
    llvm::Value* target_field_ptr, llvm::Type* field_llvm_type,
    TypeId field_type_id, OwnershipPolicy policy) {
  auto& builder = context.GetBuilder();
  const auto& types = context.GetTypeArena();
  const Type& field_type = types[field_type_id];

  if (field_type.Kind() == TypeKind::kString) {
    auto* ptr_ty = llvm::PointerType::getUnqual(context.GetLlvmContext());

    // 1. Load source string handle
    llvm::Value* new_val =
        builder.CreateLoad(ptr_ty, source_field_ptr, "sf.src");

    // 2. Apply ownership policy
    if (policy == OwnershipPolicy::kClone) {
      new_val = builder.CreateCall(context.GetLyraStringRetain(), {new_val});
    } else {
      // Move: null out source field to transfer ownership
      auto* null_val =
          llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptr_ty));
      builder.CreateStore(null_val, source_field_ptr);
    }

    // 3. Destroy old target value
    Destroy(context, target_field_ptr, field_type_id);

    // 4. Store new value
    builder.CreateStore(new_val, target_field_ptr);
    return;
  }

  if (field_type.Kind() == TypeKind::kUnpackedStruct) {
    // Nested struct: check if it contains managed fields and recurse if so
    if (NeedsFieldByField(field_type_id, types)) {
      AssignStructFieldByField(
          context, source_field_ptr, target_field_ptr, field_type_id, policy);
    } else {
      // No managed fields: aggregate load/store
      llvm::Value* val =
          builder.CreateLoad(field_llvm_type, source_field_ptr, "sf.agg");
      builder.CreateStore(val, target_field_ptr);
    }
    return;
  }

  // Other fields: simple load/store (no ownership semantics)
  llvm::Value* val =
      builder.CreateLoad(field_llvm_type, source_field_ptr, "sf.val");
  builder.CreateStore(val, target_field_ptr);
}

// Assign struct field-by-field for structs containing string fields.
void AssignStructFieldByField(
    Context& context, llvm::Value* source_ptr, llvm::Value* target_ptr,
    TypeId struct_type_id, OwnershipPolicy policy) {
  auto& builder = context.GetBuilder();
  const auto& types = context.GetTypeArena();
  const Type& struct_type = types[struct_type_id];
  const auto& struct_info = struct_type.AsUnpackedStruct();

  // Get LLVM struct type for GEP operations
  llvm::Type* llvm_struct_type =
      BuildLlvmTypeForTypeId(context.GetLlvmContext(), struct_type_id, types);
  auto* llvm_struct = llvm::cast<llvm::StructType>(llvm_struct_type);

  for (size_t i = 0; i < struct_info.fields.size(); ++i) {
    const auto& field = struct_info.fields[i];
    auto field_idx = static_cast<unsigned>(i);

    llvm::Value* src_field_ptr =
        builder.CreateStructGEP(llvm_struct_type, source_ptr, field_idx);
    llvm::Value* tgt_field_ptr =
        builder.CreateStructGEP(llvm_struct_type, target_ptr, field_idx);
    llvm::Type* field_llvm_type = llvm_struct->getElementType(field_idx);

    AssignField(
        context, src_field_ptr, tgt_field_ptr, field_llvm_type, field.type,
        policy);
  }
}

}  // namespace

void AssignStruct(
    Context& context, mir::PlaceId target, const mir::Operand& source,
    OwnershipPolicy policy, TypeId struct_type_id) {
  auto& builder = context.GetBuilder();
  const auto& types = context.GetTypeArena();

  llvm::Value* target_ptr = context.GetPlacePointer(target);
  llvm::Type* storage_type = context.GetPlaceLlvmType(target);

  // Container handles (dynarray/queue) require deep copy not yet implemented
  if (TypeContainsManaged(struct_type_id, types) &&
      !NeedsFieldByField(struct_type_id, types)) {
    // Contains containers but not strings - this is the deferred case
    throw common::UnsupportedErrorException(
        common::UnsupportedLayer::kMirToLlvm, common::UnsupportedKind::kFeature,
        context.GetCurrentOrigin(),
        "unpacked struct assignment with container fields "
        "(dynamic array/queue) not yet supported");
  }

  // Structs with string fields require field-by-field assignment
  if (NeedsFieldByField(struct_type_id, types)) {
    // Design slots with string-containing structs not yet supported
    if (IsDesignPlace(context, target)) {
      throw common::UnsupportedErrorException(
          common::UnsupportedLayer::kMirToLlvm,
          common::UnsupportedKind::kFeature, context.GetCurrentOrigin(),
          "unpacked struct assignment to design slot with string fields "
          "not yet supported");
    }

    // Get source pointer (source must be a place for struct assignment)
    if (!std::holds_alternative<mir::PlaceId>(source.payload)) {
      throw common::InternalError(
          "AssignStruct",
          "struct assignment source must be a place, not a constant");
    }
    auto src_place_id = std::get<mir::PlaceId>(source.payload);
    llvm::Value* source_ptr = context.GetPlacePointer(src_place_id);

    AssignStructFieldByField(
        context, source_ptr, target_ptr, struct_type_id, policy);
    return;
  }

  // No managed fields: fast aggregate load/store
  llvm::Value* val = LowerOperandRaw(context, source);
  if (IsDesignPlace(context, target)) {
    StoreDesignWithNotify(context, val, target_ptr, storage_type, target);
  } else {
    builder.CreateStore(val, target_ptr);
  }
}

}  // namespace lyra::lowering::mir_to_llvm
