#pragma once

#include <vector>

#include "absl/container/flat_hash_map.h"
#include "absl/hash/hash.h"
#include "lyra/common/type.hpp"

namespace lyra {

struct TypeKeyHash {
  auto operator()(const TypeKey& key) const -> size_t {
    return absl::HashOf(key.kind, key.payload);
  }
};

class TypeArena final {
 public:
  TypeArena() = default;
  ~TypeArena() = default;

  TypeArena(const TypeArena&) = delete;
  auto operator=(const TypeArena&) -> TypeArena& = delete;

  TypeArena(TypeArena&&) = default;
  auto operator=(TypeArena&&) -> TypeArena& = default;

  auto Intern(TypeKind kind, TypePayload payload) -> TypeId;

  [[nodiscard]] auto operator[](TypeId id) const -> const Type&;

  // Intern a field for a specific type and ordinal. Idempotent - returns
  // existing FieldId if already interned, or creates a new one.
  // Call this during type lowering after interning a struct/union type.
  auto InternField(TypeId type, uint32_t ordinal, FieldInfo info) -> FieldId;

  // Get the FieldId for a type's field by ordinal. Returns kInvalidFieldId
  // if not found (should not happen if type was properly lowered).
  [[nodiscard]] auto GetFieldId(TypeId type, uint32_t ordinal) const -> FieldId;

  // Lookup field info by ID.
  [[nodiscard]] auto GetField(FieldId id) const -> const FieldInfo&;

 private:
  std::vector<Type> types_;
  absl::flat_hash_map<TypeKey, TypeId, TypeKeyHash> map_;
  std::vector<FieldInfo> fields_;
  absl::flat_hash_map<std::pair<TypeId, uint32_t>, FieldId> field_id_map_;
};

// Total bit width for packed types (kIntegral, kPackedArray, kPackedStruct,
// kEnum). For kIntegral: returns bit_width
// For kPackedArray: returns PackedBitWidth(element_type) * range.Size()
// For kPackedStruct: returns total_bit_width
// For kEnum: returns bit width of base type
// Throws on non-packed types.
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

// Element width for packed arrays - ONLY accepts kPackedArray.
// Returns PackedBitWidth of the element type.
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

// Get the base IntegralInfo for a packed type (kIntegral, kPackedArray, or
// kEnum). For kIntegral: returns the IntegralInfo directly
// For kPackedArray: recursively finds the base integral element type
// For kEnum: returns the base type's IntegralInfo
// Does NOT support kPackedStruct (use IsPackedSigned/IsPackedFourState
// instead).
inline auto PackedBaseInfo(const Type& type, const TypeArena& arena)
    -> const IntegralInfo& {
  if (type.Kind() != TypeKind::kIntegral &&
      type.Kind() != TypeKind::kPackedArray && type.Kind() != TypeKind::kEnum) {
    throw common::InternalError(
        "PackedBaseInfo", std::format(
                              "expected Integral/PackedArray/Enum, got {}",
                              ToString(type.Kind())));
  }
  if (type.Kind() == TypeKind::kIntegral) {
    return type.AsIntegral();
  }
  if (type.Kind() == TypeKind::kEnum) {
    const auto& enum_info = type.AsEnum();
    return PackedBaseInfo(arena[enum_info.base_type], arena);
  }
  const auto& info = type.AsPackedArray();
  return PackedBaseInfo(arena[info.element_type], arena);
}

// Check if a packed type is signed.
// For kIntegral: returns is_signed
// For kPackedArray: returns the base element's signedness
// For kPackedStruct: returns is_signed
// For kEnum: returns the base type's signedness
inline auto IsPackedSigned(const Type& type, const TypeArena& arena) -> bool {
  if (type.Kind() == TypeKind::kPackedStruct) {
    return type.AsPackedStruct().is_signed;
  }
  return PackedBaseInfo(type, arena).is_signed;
}

// Check if a packed type is 4-state.
// For kIntegral: returns is_four_state
// For kPackedArray: returns the base element's 4-state nature
// For kPackedStruct: returns is_four_state
// For kEnum: returns the base type's 4-state nature
inline auto IsPackedFourState(const Type& type, const TypeArena& arena)
    -> bool {
  if (type.Kind() == TypeKind::kPackedStruct) {
    return type.AsPackedStruct().is_four_state;
  }
  return PackedBaseInfo(type, arena).is_four_state;
}

}  // namespace lyra
