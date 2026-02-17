#include "lyra/llvm_backend/compute/rvalue.hpp"

#include <expected>
#include <format>

#include <llvm/IR/DerivedTypes.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/layout/layout.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/place_type.hpp"

namespace lyra::lowering::mir_to_llvm {

auto GetTypeInfoFromType(Context& context, TypeId type_id)
    -> Result<PlaceTypeInfo> {
  const auto& types = context.GetTypeArena();
  const Type& type = types[type_id];

  // Managed handle types: string, dynamic array, queue, associative array
  // These are all represented as opaque pointers at the LLVM level.
  if (type.Kind() == TypeKind::kString ||
      type.Kind() == TypeKind::kDynamicArray ||
      type.Kind() == TypeKind::kQueue ||
      type.Kind() == TypeKind::kAssociativeArray) {
    return PlaceTypeInfo{
        .kind = PlaceKind::kString,  // Reuse kString for all managed handles
        .bit_width = 0,
        .is_four_state = false,
    };
  }

  if (!IsPacked(type)) {
    return std::unexpected(context.GetDiagnosticContext().MakeUnsupported(
        context.GetCurrentOrigin(),
        std::format("non-packed type not supported: {}", ToString(type)),
        UnsupportedCategory::kType));
  }
  return PlaceTypeInfo{
      .kind = PlaceKind::kIntegral,
      .bit_width = PackedBitWidth(type, types),
      .is_four_state = context.IsPackedFourState(type),
  };
}

auto ValidateAndGetTypeInfo(Context& context, mir::PlaceId place_id)
    -> Result<PlaceTypeInfo> {
  const auto& arena = context.GetMirArena();
  const auto& types = context.GetTypeArena();
  const auto& place = arena[place_id];
  TypeId type_id = mir::TypeOfPlace(types, place);
  return GetTypeInfoFromType(context, type_id);
}

auto GetLlvmTypeForType(Context& context, TypeId type_id)
    -> Result<llvm::Type*> {
  auto type_info_or_err = GetTypeInfoFromType(context, type_id);
  if (!type_info_or_err) return std::unexpected(type_info_or_err.error());
  const auto& info = *type_info_or_err;

  if (info.kind == PlaceKind::kString) {
    return llvm::PointerType::getUnqual(context.GetLlvmContext());
  }

  // Packed integral type
  if (info.is_four_state) {
    return context.GetPlaceLlvmType4State(info.bit_width);
  }
  return GetLlvmStorageType(context.GetLlvmContext(), info.bit_width);
}

}  // namespace lyra::lowering::mir_to_llvm
