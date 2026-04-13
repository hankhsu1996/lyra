#include "lyra/llvm_backend/compute/rvalue.hpp"

#include <expected>
#include <format>

#include <llvm/IR/DerivedTypes.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/layout/layout.hpp"
#include "lyra/llvm_backend/value_repr.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/place_type.hpp"

namespace lyra::lowering::mir_to_llvm {

auto GetTypeInfoFromType(const CuFacts& facts, Context& context, TypeId type_id)
    -> Result<PlaceTypeInfo> {
  const auto& types = *facts.types;
  const Type& type = types[type_id];

  // Managed handle types: string, dynamic array, queue, associative array.
  // Opaque pointers at the LLVM level with managed (refcounted) semantics.
  if (type.Kind() == TypeKind::kString ||
      type.Kind() == TypeKind::kDynamicArray ||
      type.Kind() == TypeKind::kQueue ||
      type.Kind() == TypeKind::kAssociativeArray) {
    return PlaceTypeInfo{
        .kind = PlaceKind::kManagedHandle,
        .bit_width = 0,
        .is_four_state = false,
    };
  }

  // Pointer-like scalar: chandle. Opaque pointer, not managed.
  if (type.Kind() == TypeKind::kChandle) {
    return PlaceTypeInfo{
        .kind = PlaceKind::kPointerScalar,
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
      .is_four_state = IsPackedFourState(facts, type),
  };
}

auto ValidateAndGetTypeInfo(
    const CuFacts& facts, Context& context, mir::PlaceId place_id)
    -> Result<PlaceTypeInfo> {
  const auto& arena = context.GetMirArena();
  const auto& types = *facts.types;
  const auto& place = arena[place_id];
  TypeId type_id = mir::TypeOfPlace(types, place);
  return GetTypeInfoFromType(facts, context, type_id);
}

auto GetLlvmTypeForType(const CuFacts& facts, Context& context, TypeId type_id)
    -> Result<llvm::Type*> {
  auto type_info_or_err = GetTypeInfoFromType(facts, context, type_id);
  if (!type_info_or_err) return std::unexpected(type_info_or_err.error());
  const auto& info = *type_info_or_err;

  if (info.kind == PlaceKind::kManagedHandle ||
      info.kind == PlaceKind::kPointerScalar) {
    return llvm::PointerType::getUnqual(context.GetLlvmContext());
  }

  // Packed integral type
  if (info.is_four_state) {
    return context.GetPlaceLlvmType4State(info.bit_width);
  }
  return GetBackingLlvmType(context.GetLlvmContext(), info.bit_width);
}

auto GetFourStatePlaneType(
    const CuFacts& facts, Context& context, TypeId type_id) -> llvm::Type* {
  if (!IsFourState(facts, type_id)) {
    throw common::InternalError(
        "GetFourStatePlaneType", std::format("type is not 4-state"));
  }
  auto type_info_or_err = GetTypeInfoFromType(facts, context, type_id);
  if (!type_info_or_err) {
    throw common::InternalError(
        "GetFourStatePlaneType",
        std::format("failed to get type info for TypeId"));
  }
  const auto& info = *type_info_or_err;
  return GetBackingLlvmType(context.GetLlvmContext(), info.bit_width);
}

}  // namespace lyra::lowering::mir_to_llvm
