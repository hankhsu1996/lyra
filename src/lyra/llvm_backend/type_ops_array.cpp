#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/operand.hpp"
#include "lyra/llvm_backend/type_ops_handlers.hpp"
#include "lyra/llvm_backend/type_ops_store.hpp"

namespace lyra::lowering::mir_to_llvm {

auto AssignArray(
    Context& context, mir::PlaceId target, const mir::Operand& source,
    TypeId array_type_id) -> Result<void> {
  auto& builder = context.GetBuilder();
  const auto& types = context.GetTypeArena();
  const Type& type = types[array_type_id];
  const auto& arr_info = type.AsUnpackedArray();
  const Type& elem_type = types[arr_info.element_type];

  auto target_ptr_or_err = context.GetPlacePointer(target);
  if (!target_ptr_or_err) return std::unexpected(target_ptr_or_err.error());
  llvm::Value* target_ptr = *target_ptr_or_err;

  auto storage_type_or_err = context.GetPlaceLlvmType(target);
  if (!storage_type_or_err) return std::unexpected(storage_type_or_err.error());
  llvm::Type* storage_type = *storage_type_or_err;

  if (elem_type.Kind() == TypeKind::kDynamicArray ||
      elem_type.Kind() == TypeKind::kQueue ||
      elem_type.Kind() == TypeKind::kString) {
    return std::unexpected(context.GetDiagnosticContext().MakeUnsupported(
        context.GetCurrentOrigin(),
        "unpacked array assignment with owned-handle elements "
        "(dynamic array or string) not yet supported",
        UnsupportedCategory::kFeature));
  }

  auto val_or_err = LowerOperandRaw(context, source);
  if (!val_or_err) return std::unexpected(val_or_err.error());
  llvm::Value* val = *val_or_err;
  if (IsDesignPlace(context, target)) {
    StoreDesignWithNotify(context, val, target_ptr, storage_type, target);
  } else {
    builder.CreateStore(val, target_ptr);
  }
  return {};
}

}  // namespace lyra::lowering::mir_to_llvm
