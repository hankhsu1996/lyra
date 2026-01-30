#pragma once

#include <algorithm>
#include <cstdint>
#include <optional>
#include <unordered_set>
#include <vector>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_queries.hpp"

namespace lyra {

struct FieldLayout {
  TypeId type;
  uint32_t bit_offset;
  uint32_t bit_size;
};

struct Layout {
  uint32_t bit_size;
  std::vector<FieldLayout> fields;
  std::optional<uint32_t> element_stride_bits;
  std::optional<TypeId> element_type;
};

inline auto BlobBitSize(TypeId type_id, const TypeArena& types) -> uint32_t;

namespace detail {

inline auto MakeSimpleLayout(uint32_t bit_size) -> Layout {
  return Layout{
      .bit_size = bit_size,
      .fields = {},
      .element_stride_bits = std::nullopt,
      .element_type = std::nullopt};
}

}  // namespace detail

inline auto LayoutOf(
    TypeId type_id, const TypeArena& types,
    std::unordered_set<uint32_t>& visited) -> Layout {
  if (!visited.insert(type_id.value).second) {
    throw common::InternalError("LayoutOf", "recursive type in layout");
  }

  const auto& type = types[type_id];
  switch (type.Kind()) {
    case TypeKind::kUnpackedStruct: {
      const auto& info = type.AsUnpackedStruct();
      std::vector<FieldLayout> fields;
      uint32_t offset = 0;
      for (const auto& field : info.fields) {
        uint32_t field_size = BlobBitSize(field.type, types);
        fields.push_back({
            .type = field.type,
            .bit_offset = offset,
            .bit_size = field_size,
        });
        offset += field_size;
      }
      return Layout{
          .bit_size = offset,
          .fields = std::move(fields),
          .element_stride_bits = std::nullopt,
          .element_type = std::nullopt};
    }
    case TypeKind::kUnpackedArray: {
      const auto& info = type.AsUnpackedArray();
      uint32_t elem_size = BlobBitSize(info.element_type, types);
      uint32_t count = info.range.Size();
      return Layout{
          .bit_size = elem_size * count,
          .fields = {},
          .element_stride_bits = elem_size,
          .element_type = info.element_type,
      };
    }
    case TypeKind::kUnpackedUnion: {
      const auto& info = type.AsUnpackedUnion();
      return detail::MakeSimpleLayout(info.storage_bit_width);
    }
    case TypeKind::kIntegral:
      return detail::MakeSimpleLayout(type.AsIntegral().bit_width);
    case TypeKind::kPackedArray:
    case TypeKind::kPackedStruct:
    case TypeKind::kEnum:
      return detail::MakeSimpleLayout(PackedBitWidth(type, types));
    case TypeKind::kReal:
      return detail::MakeSimpleLayout(64);
    case TypeKind::kShortReal:
      return detail::MakeSimpleLayout(32);
    default:
      throw common::InternalError(
          "LayoutOf", "unsupported type kind for layout");
  }
}

inline auto LayoutOf(TypeId type_id, const TypeArena& types) -> Layout {
  std::unordered_set<uint32_t> visited;
  return LayoutOf(type_id, types, visited);
}

inline auto BlobBitSize(TypeId type_id, const TypeArena& types) -> uint32_t {
  const auto& type = types[type_id];
  switch (type.Kind()) {
    case TypeKind::kUnpackedStruct: {
      const auto& info = type.AsUnpackedStruct();
      uint32_t total = 0;
      for (const auto& field : info.fields) {
        total += BlobBitSize(field.type, types);
      }
      return total;
    }
    case TypeKind::kUnpackedArray: {
      const auto& info = type.AsUnpackedArray();
      return BlobBitSize(info.element_type, types) * info.range.Size();
    }
    case TypeKind::kUnpackedUnion: {
      const auto& info = type.AsUnpackedUnion();
      return info.storage_bit_width;
    }
    case TypeKind::kIntegral:
      return type.AsIntegral().bit_width;
    case TypeKind::kPackedArray:
    case TypeKind::kPackedStruct:
    case TypeKind::kEnum:
      return PackedBitWidth(type, types);
    case TypeKind::kReal:
      return 64;
    case TypeKind::kShortReal:
      return 32;
    default:
      throw common::InternalError(
          "BlobBitSize", "unsupported type kind for blob size");
  }
}

inline auto IsFourStateType(TypeId type_id, const TypeArena& types) -> bool {
  const auto& type = types[type_id];
  switch (type.Kind()) {
    case TypeKind::kReal:
    case TypeKind::kShortReal:
      return false;
    case TypeKind::kIntegral:
      return type.AsIntegral().is_four_state;
    case TypeKind::kPackedArray:
    case TypeKind::kPackedStruct:
    case TypeKind::kEnum:
      return IsPackedFourState(type, types);
    case TypeKind::kUnpackedStruct: {
      const auto& info = type.AsUnpackedStruct();
      return std::ranges::any_of(info.fields, [&](const auto& field) {
        return IsFourStateType(field.type, types);
      });
    }
    case TypeKind::kUnpackedArray: {
      const auto& info = type.AsUnpackedArray();
      return IsFourStateType(info.element_type, types);
    }
    case TypeKind::kUnpackedUnion: {
      const auto& info = type.AsUnpackedUnion();
      return std::ranges::any_of(info.members, [&](const auto& member) {
        return IsFourStateType(member.type, types);
      });
    }
    default:
      return false;
  }
}

inline auto ContainsFloat(TypeId type_id, const TypeArena& types) -> bool {
  const auto& type = types[type_id];
  switch (type.Kind()) {
    case TypeKind::kReal:
    case TypeKind::kShortReal:
      return true;
    case TypeKind::kUnpackedStruct: {
      const auto& info = type.AsUnpackedStruct();
      return std::ranges::any_of(info.fields, [&](const auto& field) {
        return ContainsFloat(field.type, types);
      });
    }
    case TypeKind::kUnpackedArray: {
      const auto& info = type.AsUnpackedArray();
      return ContainsFloat(info.element_type, types);
    }
    case TypeKind::kUnpackedUnion: {
      const auto& info = type.AsUnpackedUnion();
      return std::ranges::any_of(info.members, [&](const auto& member) {
        return ContainsFloat(member.type, types);
      });
    }
    default:
      return false;
  }
}

}  // namespace lyra
