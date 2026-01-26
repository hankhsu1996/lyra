#include "lyra/llvm_backend/instruction_compute_rvalue.hpp"

#include <expected>
#include <format>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/place_type.hpp"

namespace lyra::lowering::mir_to_llvm {

auto ValidateAndGetTypeInfo(Context& context, mir::PlaceId place_id)
    -> Result<PlaceTypeInfo> {
  const auto& arena = context.GetMirArena();
  const auto& types = context.GetTypeArena();
  const auto& place = arena[place_id];
  const Type& type = types[mir::TypeOfPlace(types, place)];

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

}  // namespace lyra::lowering::mir_to_llvm
