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

 private:
  std::vector<Type> types_;
  absl::flat_hash_map<TypeKey, TypeId, TypeKeyHash> map_;
};

// Total bit width for packed types (kIntegral or kPackedArray).
// For kIntegral: returns bit_width
// For kPackedArray: returns PackedBitWidth(element_type) * range.Size()
// Asserts on non-packed types.
inline auto PackedBitWidth(const Type& type, const TypeArena& arena)
    -> uint32_t {
  assert(IsPacked(type));
  if (type.Kind() == TypeKind::kIntegral) {
    return type.AsIntegral().bit_width;
  }
  const auto& info = type.AsPackedArray();
  return PackedBitWidth(arena[info.element_type], arena) * info.range.Size();
}

// Element width for packed arrays - ONLY accepts kPackedArray.
// Returns PackedBitWidth of the element type.
inline auto PackedArrayElementWidth(const Type& type, const TypeArena& arena)
    -> uint32_t {
  assert(type.Kind() == TypeKind::kPackedArray);
  const auto& info = type.AsPackedArray();
  return PackedBitWidth(arena[info.element_type], arena);
}

// Get the base IntegralInfo for a packed type.
// For kIntegral: returns the IntegralInfo directly
// For kPackedArray: recursively finds the base integral element type
// Asserts on non-packed types.
inline auto PackedBaseInfo(const Type& type, const TypeArena& arena)
    -> const IntegralInfo& {
  assert(IsPacked(type));
  if (type.Kind() == TypeKind::kIntegral) {
    return type.AsIntegral();
  }
  const auto& info = type.AsPackedArray();
  return PackedBaseInfo(arena[info.element_type], arena);
}

// Check if a packed type is signed.
// For kIntegral: returns is_signed
// For kPackedArray: returns the base element's signedness
inline auto IsPackedSigned(const Type& type, const TypeArena& arena) -> bool {
  return PackedBaseInfo(type, arena).is_signed;
}

// Check if a packed type is 4-state.
// For kIntegral: returns is_four_state
// For kPackedArray: returns the base element's 4-state nature
inline auto IsPackedFourState(const Type& type, const TypeArena& arena)
    -> bool {
  return PackedBaseInfo(type, arena).is_four_state;
}

}  // namespace lyra
