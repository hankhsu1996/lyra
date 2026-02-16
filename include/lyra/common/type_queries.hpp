#pragma once

#include <algorithm>
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

// Check if a type kind is "managed" (requires lifecycle operations).
// Managed types: string, dynamic array, queue.
inline auto IsManagedKind(TypeKind kind) -> bool {
  return kind == TypeKind::kString || kind == TypeKind::kDynamicArray ||
         kind == TypeKind::kQueue || kind == TypeKind::kAssociativeArray;
}

namespace detail {

// Generic helper: check if type (or any nested field/element) satisfies pred.
template <typename Pred>
auto TypeContains(TypeId type_id, const TypeArena& arena, Pred pred) -> bool {
  const Type& type = arena[type_id];

  if (pred(type.Kind())) {
    return true;
  }

  switch (type.Kind()) {
    case TypeKind::kUnpackedStruct: {
      const auto& info = type.AsUnpackedStruct();
      return std::ranges::any_of(info.fields, [&](const auto& field) {
        return TypeContains(field.type, arena, pred);
      });
    }
    case TypeKind::kUnpackedArray:
      return TypeContains(type.AsUnpackedArray().element_type, arena, pred);
    default:
      return false;
  }
}

}  // namespace detail

// Check if a type (or any nested field/element) contains managed types.
// Used to determine if a type requires destroy/clone operations.
inline auto TypeContainsManaged(TypeId type_id, const TypeArena& arena)
    -> bool {
  return detail::TypeContains(
      type_id, arena, [](TypeKind kind) { return IsManagedKind(kind); });
}

// Check if a type (or any nested field/element) contains strings.
// Used to determine if a type requires field-by-field assignment.
inline auto TypeContainsString(TypeId type_id, const TypeArena& arena) -> bool {
  return detail::TypeContains(
      type_id, arena, [](TypeKind kind) { return kind == TypeKind::kString; });
}

}  // namespace lyra

// Re-export in common namespace for explicit qualification
namespace lyra::common {
using lyra::IsManagedKind;
using lyra::TypeContainsManaged;
using lyra::TypeContainsString;
}  // namespace lyra::common
