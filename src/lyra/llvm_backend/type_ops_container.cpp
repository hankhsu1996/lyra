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

  // 3. Get WriteTarget for unified pointer + signal_id
  auto wt_or_err = context.GetWriteTarget(target);
  if (!wt_or_err) return std::unexpected(wt_or_err.error());
  const WriteTarget& wt = *wt_or_err;

  // 4. Store using centralized helper (handles destroy-old, store, notify)
  StoreDynArrayToWriteTarget(context, new_handle, wt, type_id);
  return {};
}

}  // namespace lyra::lowering::mir_to_llvm
