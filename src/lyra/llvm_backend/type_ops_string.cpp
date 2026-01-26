#include <expected>
#include <variant>

#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/operand.hpp"
#include "lyra/llvm_backend/type_ops_handlers.hpp"
#include "lyra/llvm_backend/type_ops_store.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/place.hpp"

namespace lyra::lowering::mir_to_llvm {

auto AssignString(
    Context& context, mir::PlaceId target, const mir::Operand& source,
    OwnershipPolicy policy, TypeId type_id) -> Result<void> {
  auto& builder = context.GetBuilder();
  const auto& arena = context.GetMirArena();

  auto target_ptr_result = context.GetPlacePointer(target);
  if (!target_ptr_result) return std::unexpected(target_ptr_result.error());
  llvm::Value* target_ptr = *target_ptr_result;

  // 1. Get new value
  auto new_val_or_err = LowerOperand(context, source);
  if (!new_val_or_err) return std::unexpected(new_val_or_err.error());
  llvm::Value* new_val = *new_val_or_err;

  // 2. Apply ownership policy (retain for clone, null-out source for move)
  if (policy == OwnershipPolicy::kClone) {
    new_val = builder.CreateCall(context.GetLyraStringRetain(), {new_val});
  } else if (std::holds_alternative<mir::PlaceId>(source.payload)) {
    auto src_place_id = std::get<mir::PlaceId>(source.payload);
    const auto& src_place = arena[src_place_id];
    if (src_place.root.kind == mir::PlaceRoot::Kind::kTemp) {
      auto* ptr_ty = llvm::PointerType::getUnqual(context.GetLlvmContext());
      auto src_ptr_result = context.GetPlacePointer(src_place_id);
      if (!src_ptr_result) return std::unexpected(src_ptr_result.error());
      llvm::Value* src_ptr = *src_ptr_result;
      auto* null_val =
          llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptr_ty));
      builder.CreateStore(null_val, src_ptr);
    }
  }

  // 3. Destroy old target value
  // Note: For design slots, we load old before helper call to preserve across
  // notify. The helper handles the store+notify atomically.
  if (IsDesignPlace(context, target)) {
    auto storage_type_or_err = context.GetPlaceLlvmType(target);
    if (!storage_type_or_err)
      return std::unexpected(storage_type_or_err.error());
    llvm::Type* storage_type = *storage_type_or_err;
    auto* old_val = builder.CreateLoad(storage_type, target_ptr);
    auto signal_id = GetSignalId(context, target);
    auto* i32_ty = llvm::Type::getInt32Ty(context.GetLlvmContext());
    builder.CreateCall(
        context.GetLyraStoreString(),
        {context.GetEnginePointer(), target_ptr, new_val,
         llvm::ConstantInt::get(i32_ty, signal_id)});
    builder.CreateCall(context.GetLyraStringRelease(), {old_val});
  } else {
    // Non-design: Destroy then store
    Destroy(context, target_ptr, type_id);
    builder.CreateStore(new_val, target_ptr);
  }
  return {};
}

}  // namespace lyra::lowering::mir_to_llvm
