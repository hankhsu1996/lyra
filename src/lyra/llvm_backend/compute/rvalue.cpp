#include "lyra/llvm_backend/compute/rvalue.hpp"

#include <expected>
#include <format>

#include <llvm/IR/DerivedTypes.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_queries.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/place_type.hpp"

namespace lyra::lowering::mir_to_llvm {

auto GetTypeInfoFromType(Context& context, TypeId type_id)
    -> Result<PlaceTypeInfo> {
  const auto& types = context.GetTypeArena();
  const Type& type = types[type_id];

  if (type.Kind() == TypeKind::kString) {
    return PlaceTypeInfo{
        .kind = PlaceKind::kString,
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
      .is_four_state = IsPackedFourState(type, types),
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
  return llvm::Type::getIntNTy(context.GetLlvmContext(), info.bit_width);
}

}  // namespace lyra::lowering::mir_to_llvm
