#include "lyra/common/unsupported_error.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/operand.hpp"
#include "lyra/llvm_backend/type_ops_handlers.hpp"
#include "lyra/llvm_backend/type_ops_store.hpp"

namespace lyra::lowering::mir_to_llvm {

void AssignArray(
    Context& context, mir::PlaceId target, const mir::Operand& source,
    TypeId array_type_id) {
  auto& builder = context.GetBuilder();
  const auto& types = context.GetTypeArena();
  const Type& type = types[array_type_id];
  const auto& arr_info = type.AsUnpackedArray();
  const Type& elem_type = types[arr_info.element_type];

  llvm::Value* target_ptr = context.GetPlacePointer(target);
  llvm::Type* storage_type = context.GetPlaceLlvmType(target);

  if (elem_type.Kind() == TypeKind::kDynamicArray ||
      elem_type.Kind() == TypeKind::kQueue ||
      elem_type.Kind() == TypeKind::kString) {
    throw common::UnsupportedErrorException(
        common::UnsupportedLayer::kMirToLlvm, common::UnsupportedKind::kFeature,
        context.GetCurrentOrigin(),
        "unpacked array assignment with owned-handle elements "
        "(dynamic array or string) not yet supported");
  }

  llvm::Value* val = LowerOperandRaw(context, source);
  if (IsDesignPlace(context, target)) {
    StoreDesignWithNotify(context, val, target_ptr, storage_type, target);
  } else {
    builder.CreateStore(val, target_ptr);
  }
}

}  // namespace lyra::lowering::mir_to_llvm
