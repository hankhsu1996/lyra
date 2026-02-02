#pragma once

#include <cstdint>
#include <optional>
#include <string>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/interp/runtime_value.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/place_type.hpp"
#include "lyra/semantic/format.hpp"

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

// Safely extract index from RuntimeValue, return nullopt if X/Z.
// Per IEEE 1800-2023, invalid index means no-op (not an error).
inline auto TryGetIndex(
    const RuntimeValue& val, const TypeArena& types, TypeId type_id)
    -> std::optional<int64_t> {
  if (!IsIntegral(val)) {
    return std::nullopt;
  }
  const auto& integral = AsIntegral(val);
  if (!integral.IsKnown()) {
    return std::nullopt;  // X/Z -> invalid
  }

  // Extract raw bits
  uint64_t raw_bits = integral.value.empty() ? 0 : integral.value[0];

  // Sign-extend if operand type is signed
  const auto& type_info = types[type_id];
  if (type_info.Kind() == TypeKind::kIntegral) {
    const auto& info = type_info.AsIntegral();
    if (info.is_signed && info.bit_width < 64) {
      uint64_t sign_bit = 1ULL << (info.bit_width - 1);
      if ((raw_bits & sign_bit) != 0) {
        uint64_t mask = ~((1ULL << info.bit_width) - 1);
        raw_bits |= mask;
      }
    }
  }

  return static_cast<int64_t>(raw_bits);
}

// Coerce a RuntimeValue to std::string.
// - If string type: returns the string value directly.
// - If packed integral: converts via semantic::PackedToStringBytes (MSB-first).
// - Otherwise: throws InternalError.
// This is the canonical helper for string-like argument coercion in the
// interpreter (matches LowerArgAsStringHandle in LLVM lowering).
inline auto CoerceToString(const RuntimeValue& val, const char* context)
    -> std::string {
  if (IsString(val)) {
    return AsString(val).value;
  }
  if (IsIntegral(val)) {
    return semantic::PackedToStringBytes(AsIntegral(val));
  }
  throw common::InternalError(context, "value must be string or packed");
}

}  // namespace lyra::mir::interp
