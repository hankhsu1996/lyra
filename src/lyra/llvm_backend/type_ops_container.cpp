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

auto AssignDynArray(
    Context& context, mir::PlaceId target, const mir::Operand& source,
    OwnershipPolicy policy, TypeId type_id) -> Result<void> {
  auto& builder = context.GetBuilder();
  const auto& arena = context.GetMirArena();

  auto* ptr_ty = llvm::PointerType::getUnqual(context.GetLlvmContext());
  auto target_ptr_result = context.GetPlacePointer(target);
  if (!target_ptr_result) return std::unexpected(target_ptr_result.error());
  llvm::Value* target_ptr = *target_ptr_result;

  // 1. Get new value
  auto new_handle_or_err = LowerOperand(context, source);
  if (!new_handle_or_err) return std::unexpected(new_handle_or_err.error());
  llvm::Value* new_handle = *new_handle_or_err;

  // 2. Apply ownership policy
  if (policy == OwnershipPolicy::kClone) {
    new_handle =
        builder.CreateCall(context.GetLyraDynArrayClone(), {new_handle});
  } else if (std::holds_alternative<mir::PlaceId>(source.payload)) {
    auto src_place_id = std::get<mir::PlaceId>(source.payload);
    const auto& src_place = arena[src_place_id];
    if (src_place.root.kind == mir::PlaceRoot::Kind::kTemp) {
      auto src_ptr_result = context.GetPlacePointer(src_place_id);
      if (!src_ptr_result) return std::unexpected(src_ptr_result.error());
      auto* null_val =
          llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptr_ty));
      builder.CreateStore(null_val, *src_ptr_result);
    }
  }
  // else: source is a Const (nullptr literal) - use as-is

  // 3. Destroy old + 4. Store new
  // Note: For design slots, load old before helper to preserve across notify
  if (IsDesignPlace(context, target)) {
    auto* old_handle = builder.CreateLoad(ptr_ty, target_ptr, "da.old");
    auto signal_id = GetSignalId(context, target);
    auto* i32_ty = llvm::Type::getInt32Ty(context.GetLlvmContext());
    builder.CreateCall(
        context.GetLyraStoreDynArray(),
        {context.GetEnginePointer(), target_ptr, new_handle,
         llvm::ConstantInt::get(i32_ty, signal_id)});
    builder.CreateCall(context.GetLyraDynArrayRelease(), {old_handle});
  } else {
    // Non-design: Destroy then store
    Destroy(context, target_ptr, type_id);
    builder.CreateStore(new_handle, target_ptr);
  }
  return {};
}

}  // namespace lyra::lowering::mir_to_llvm
