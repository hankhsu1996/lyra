#include <variant>

#include "llvm/IR/Constants.h"
#include "lyra/common/internal_error.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/type_ops_handlers.hpp"
#include "lyra/llvm_backend/type_ops_store.hpp"
#include "lyra/mir/place_type.hpp"

namespace lyra::lowering::mir_to_llvm {

void AssignUnion(
    Context& context, mir::PlaceId target, const mir::Operand& source,
    TypeId union_type_id) {
  auto& builder = context.GetBuilder();
  const auto& types = context.GetTypeArena();

  llvm::Value* target_ptr = context.GetPlacePointer(target);
  auto info = GetUnionStorageInfo(context, union_type_id);

  // Source must be a PlaceId for union assignment
  const auto* src_place_id = std::get_if<mir::PlaceId>(&source.payload);
  if (src_place_id == nullptr) {
    throw common::InternalError(
        "AssignUnion",
        "union assignment source must be a place operand (constants/rvalues "
        "not supported)");
  }

  // Source must be union-typed (not a member view)
  TypeId src_type =
      mir::TypeOfPlace(types, context.GetMirArena()[*src_place_id]);
  if (src_type != union_type_id) {
    throw common::InternalError(
        "AssignUnion", "source place type does not match target union type");
  }

  llvm::Value* source_ptr = context.GetPlacePointer(*src_place_id);

  // memcpy from source to target
  builder.CreateMemCpy(
      target_ptr, llvm::Align(info.align), source_ptr, llvm::Align(info.align),
      info.size);

  // Notify if this is a design slot
  if (IsDesignPlace(context, target)) {
    auto signal_id = GetSignalId(context, target);
    auto* i32_ty = llvm::Type::getInt32Ty(context.GetLlvmContext());
    builder.CreateCall(
        context.GetLyraStorePacked(),
        {context.GetEnginePointer(), target_ptr,
         target_ptr,  // For unions, source = target after memcpy
         llvm::ConstantInt::get(i32_ty, info.size),
         llvm::ConstantInt::get(i32_ty, signal_id)});
  }
}

void ConstructDefaultUnion(
    Context& context, llvm::Value* ptr, TypeId union_type_id) {
  auto& builder = context.GetBuilder();
  const auto& types = context.GetTypeArena();
  const Type& type = types[union_type_id];
  const auto& union_info = type.AsUnpackedUnion();
  auto info = GetUnionStorageInfo(context, union_type_id);

  // 1. Zero entire storage
  builder.CreateMemSet(
      ptr, builder.getInt8(0), info.size, llvm::Align(info.align));

  // 2. Default-construct member0 at the same location
  // For simple types, zero is already the default. For aggregate types,
  // we would need to recursively construct, but for now zero is sufficient
  // for 2-state types (which is the scope of this PR).
  if (union_info.members.empty()) {
    return;  // Empty union (shouldn't happen)
  }

  // For integral/real types, zero is already correct.
  // For struct/array members, we would need to call ConstructDefault,
  // but since all members overlay at offset 0, zero-init suffices for
  // 2-state types as long as the first member's default is zero-valued.
}

}  // namespace lyra::lowering::mir_to_llvm
