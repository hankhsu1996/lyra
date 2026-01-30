#pragma once

#include <format>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"

namespace lyra {

// Total bit width for packed types (kIntegral, kPackedArray, kPackedStruct,
// kEnum). Throws on non-packed types.
inline auto PackedBitWidth(const Type& type, const TypeArena& arena)
    -> uint32_t {
  if (!IsPacked(type)) {
    throw common::InternalError(
        "PackedBitWidth",
        std::format("expected packed type, got {}", ToString(type.Kind())));
  }
  if (type.Kind() == TypeKind::kIntegral) {
    return type.AsIntegral().bit_width;
  }
  if (type.Kind() == TypeKind::kPackedStruct) {
    return type.AsPackedStruct().total_bit_width;
  }
  if (type.Kind() == TypeKind::kEnum) {
    const auto& info = type.AsEnum();
    return PackedBitWidth(arena[info.base_type], arena);
  }
  const auto& info = type.AsPackedArray();
  return PackedBitWidth(arena[info.element_type], arena) * info.range.Size();
}

// Element width for packed arrays. Only accepts kPackedArray.
inline auto PackedArrayElementWidth(const Type& type, const TypeArena& arena)
    -> uint32_t {
  if (type.Kind() != TypeKind::kPackedArray) {
    throw common::InternalError(
        "PackedArrayElementWidth",
        std::format("expected PackedArray, got {}", ToString(type.Kind())));
  }
  const auto& info = type.AsPackedArray();
  return PackedBitWidth(arena[info.element_type], arena);
}

inline auto IsPackedSigned(const Type& type, const TypeArena& arena) -> bool {
  switch (type.Kind()) {
    case TypeKind::kIntegral:
      return type.AsIntegral().is_signed;
    case TypeKind::kPackedStruct:
      return type.AsPackedStruct().is_signed;
    case TypeKind::kEnum:
      return IsPackedSigned(arena[type.AsEnum().base_type], arena);
    case TypeKind::kPackedArray:
      return IsPackedSigned(arena[type.AsPackedArray().element_type], arena);
    default:
      throw common::InternalError(
          "IsPackedSigned",
          std::format("expected packed type, got {}", ToString(type.Kind())));
  }
}

inline auto IsPackedFourState(const Type& type, const TypeArena& arena)
    -> bool {
  switch (type.Kind()) {
    case TypeKind::kIntegral:
      return type.AsIntegral().is_four_state;
    case TypeKind::kPackedStruct:
      return type.AsPackedStruct().is_four_state;
    case TypeKind::kEnum:
      return IsPackedFourState(arena[type.AsEnum().base_type], arena);
    case TypeKind::kPackedArray:
      return IsPackedFourState(arena[type.AsPackedArray().element_type], arena);
    default:
      throw common::InternalError(
          "IsPackedFourState",
          std::format("expected packed type, got {}", ToString(type.Kind())));
  }
}

// Predicate for packed integral-like types eligible for fill operations.
inline auto IsPackedIntegralLike(TypeId type_id, const TypeArena& arena)
    -> bool {
  const Type& type = arena[type_id];
  return type.Kind() == TypeKind::kIntegral ||
         type.Kind() == TypeKind::kPackedArray;
}

// Result of computing packed fill shape from a target type.
struct PackedFillShape {
  TypeId unit_type = kInvalidTypeId;
  size_t count = 0;
  uint32_t unit_bits = 0;
  uint32_t total_bits = 0;
};

// Compute packed fill shape from target type.
// For kIntegral: unit is the provided bit_type, count = bit_width.
// For kPackedArray: unit is element type, count = range.Size().
// Always returns a valid unit_type (caller provides bit_type for kIntegral
// case).
inline auto ComputePackedFillShape(
    TypeId target_type_id, const TypeArena& arena, TypeId bit_type)
    -> PackedFillShape {
  const Type& type = arena[target_type_id];

  if (type.Kind() == TypeKind::kIntegral) {
    const auto& info = type.AsIntegral();
    return PackedFillShape{
        .unit_type = bit_type,
        .count = info.bit_width,
        .unit_bits = 1,
        .total_bits = info.bit_width};
  }

  if (type.Kind() == TypeKind::kPackedArray) {
    const auto& arr = type.AsPackedArray();
    uint32_t elem_bits = PackedBitWidth(arena[arr.element_type], arena);
    return PackedFillShape{
        .unit_type = arr.element_type,
        .count = arr.range.Size(),
        .unit_bits = elem_bits,
        .total_bits = elem_bits * arr.range.Size()};
  }

  throw common::InternalError(
      "ComputePackedFillShape",
      "target must be packed (integral or packed array)");
}

}  // namespace lyra
