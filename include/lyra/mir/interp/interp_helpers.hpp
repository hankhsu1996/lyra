#pragma once

#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/place_type.hpp"

namespace lyra::mir::interp {

// Gets the TypeId of an Operand.
inline auto TypeOfOperand(
    const Operand& op, const Arena& arena, const TypeArena& types) -> TypeId {
  switch (op.kind) {
    case Operand::Kind::kConst:
      return std::get<Constant>(op.payload).type;
    case Operand::Kind::kUse: {
      PlaceId place_id = std::get<PlaceId>(op.payload);
      const auto& place = arena[place_id];
      return TypeOfPlace(types, place);
    }
    case Operand::Kind::kPoison:
      return TypeId{};
  }
  return TypeId{};
}

// Checks if a type is signed (handles both kIntegral and packed types).
inline auto IsSignedType(const TypeArena& types, TypeId type_id) -> bool {
  if (!type_id) {
    return false;
  }
  const auto& type = types[type_id];
  if (type.Kind() == TypeKind::kIntegral) {
    return type.AsIntegral().is_signed;
  }
  if (IsPacked(type)) {
    return IsPackedSigned(type, types);
  }
  return false;
}

// Backward-compatible alias for IsSignedType.
inline auto IsSignedIntegral(const TypeArena& types, TypeId type_id) -> bool {
  return IsSignedType(types, type_id);
}

}  // namespace lyra::mir::interp
