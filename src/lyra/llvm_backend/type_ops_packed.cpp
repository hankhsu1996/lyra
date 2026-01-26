#include <expected>

#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/operand.hpp"
#include "lyra/llvm_backend/type_ops_handlers.hpp"
#include "lyra/llvm_backend/type_ops_store.hpp"

namespace lyra::lowering::mir_to_llvm {

auto AssignFourState(
    Context& context, mir::PlaceId target, const mir::Operand& source,
    llvm::StructType* struct_type) -> Result<void> {
  auto& builder = context.GetBuilder();
  auto* elem_type = struct_type->getElementType(0);

  auto target_ptr_or_err = context.GetPlacePointer(target);
  if (!target_ptr_or_err) return std::unexpected(target_ptr_or_err.error());
  llvm::Value* target_ptr = *target_ptr_or_err;

  // Load source as raw value (struct if 4-state, integer if 2-state)
  auto raw_or_err = LowerOperandRaw(context, source);
  if (!raw_or_err) return std::unexpected(raw_or_err.error());
  llvm::Value* raw = *raw_or_err;

  llvm::Value* val = nullptr;
  llvm::Value* unk = nullptr;
  if (raw->getType()->isStructTy()) {
    // Source is 4-state: pass through
    val = builder.CreateExtractValue(raw, 0, "assign.val");
    unk = builder.CreateExtractValue(raw, 1, "assign.unk");
  } else {
    // Source is 2-state: wrap as {value, unknown=0}
    val = builder.CreateZExtOrTrunc(raw, elem_type, "assign.val");
    unk = llvm::ConstantInt::get(elem_type, 0);
  }

  // Coerce to elem_type if width differs
  val = builder.CreateZExtOrTrunc(val, elem_type, "assign.val.fit");
  unk = builder.CreateZExtOrTrunc(unk, elem_type, "assign.unk.fit");

  llvm::Value* packed = llvm::UndefValue::get(struct_type);
  packed = builder.CreateInsertValue(packed, val, 0);
  packed = builder.CreateInsertValue(packed, unk, 1);
  if (IsDesignPlace(context, target)) {
    StoreDesignWithNotify(context, packed, target_ptr, struct_type, target);
  } else {
    builder.CreateStore(packed, target_ptr);
  }
  return {};
}

auto AssignTwoState(
    Context& context, mir::PlaceId target, const mir::Operand& source,
    TypeId type_id) -> Result<void> {
  auto& builder = context.GetBuilder();
  const auto& types = context.GetTypeArena();
  const Type& type = types[type_id];

  auto target_ptr_or_err = context.GetPlacePointer(target);
  if (!target_ptr_or_err) return std::unexpected(target_ptr_or_err.error());
  llvm::Value* target_ptr = *target_ptr_or_err;
  auto storage_type_or_err = context.GetPlaceLlvmType(target);
  if (!storage_type_or_err) return std::unexpected(storage_type_or_err.error());
  llvm::Type* storage_type = *storage_type_or_err;

  // Lower source (coerces 4-state to integer)
  auto source_value_or_err = LowerOperand(context, source);
  if (!source_value_or_err) return std::unexpected(source_value_or_err.error());
  llvm::Value* source_value = *source_value_or_err;

  // Adjust the value to match storage type if needed (only for integrals)
  if (source_value->getType() != storage_type &&
      source_value->getType()->isIntegerTy() && storage_type->isIntegerTy()) {
    if (type.Kind() == TypeKind::kIntegral && type.AsIntegral().is_signed) {
      source_value = builder.CreateSExtOrTrunc(source_value, storage_type);
    } else {
      source_value = builder.CreateZExtOrTrunc(source_value, storage_type);
    }
  }

  // Store to the place (with notify if design)
  if (IsDesignPlace(context, target)) {
    StoreDesignWithNotify(
        context, source_value, target_ptr, storage_type, target);
  } else {
    builder.CreateStore(source_value, target_ptr);
  }
  return {};
}

}  // namespace lyra::lowering::mir_to_llvm
