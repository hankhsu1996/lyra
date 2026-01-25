#include <variant>

#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/operand.hpp"
#include "lyra/llvm_backend/type_ops_handlers.hpp"
#include "lyra/llvm_backend/type_ops_store.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/place.hpp"

namespace lyra::lowering::mir_to_llvm {

void AssignString(
    Context& context, mir::PlaceId target, const mir::Operand& source,
    OwnershipPolicy policy, TypeId type_id) {
  auto& builder = context.GetBuilder();
  const auto& arena = context.GetMirArena();

  llvm::Value* target_ptr = context.GetPlacePointer(target);

  // 1. Get new value
  llvm::Value* new_val = LowerOperand(context, source);

  // 2. Apply ownership policy (retain for clone, null-out source for move)
  if (policy == OwnershipPolicy::kClone) {
    new_val = builder.CreateCall(context.GetLyraStringRetain(), {new_val});
  } else if (std::holds_alternative<mir::PlaceId>(source.payload)) {
    auto src_place_id = std::get<mir::PlaceId>(source.payload);
    const auto& src_place = arena[src_place_id];
    if (src_place.root.kind == mir::PlaceRoot::Kind::kTemp) {
      auto* ptr_ty = llvm::PointerType::getUnqual(context.GetLlvmContext());
      auto* src_ptr = context.GetPlacePointer(src_place_id);
      auto* null_val =
          llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptr_ty));
      builder.CreateStore(null_val, src_ptr);
    }
  }

  // 3. Destroy old target value
  // Note: For design slots, we load old before helper call to preserve across
  // notify. The helper handles the store+notify atomically.
  if (IsDesignPlace(context, target)) {
    llvm::Type* storage_type = context.GetPlaceLlvmType(target);
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
}

}  // namespace lyra::lowering::mir_to_llvm
