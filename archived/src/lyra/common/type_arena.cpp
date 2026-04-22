#include "lyra/common/type_arena.hpp"

#include <cstdint>
#include <format>
#include <utility>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"

namespace lyra {

namespace {

auto DescribePayload(TypeKind kind, const TypePayload& payload) -> std::string {
  switch (kind) {
    case TypeKind::kIntegral: {
      const auto& info = std::get<IntegralInfo>(payload);
      return std::format(
          "integral width={} signed={} 4state={}", info.bit_width,
          info.is_signed, info.is_four_state);
    }
    case TypeKind::kPackedArray: {
      const auto& info = std::get<PackedArrayInfo>(payload);
      return std::format(
          "packed_array element=type#{} range=[{}:{}]", info.element_type.value,
          info.range.left, info.range.right);
    }
    case TypeKind::kPackedStruct: {
      const auto& info = std::get<PackedStructInfo>(payload);
      return std::format(
          "packed_struct fields={} width={} signed={} 4state={}",
          info.fields.size(), info.total_bit_width, info.is_signed,
          info.is_four_state);
    }
    case TypeKind::kUnpackedArray: {
      const auto& info = std::get<UnpackedArrayInfo>(payload);
      return std::format(
          "unpacked_array element=type#{} range=[{}:{}]",
          info.element_type.value, info.range.left, info.range.right);
    }
    case TypeKind::kUnpackedStruct: {
      const auto& info = std::get<UnpackedStructInfo>(payload);
      return std::format("unpacked_struct fields={}", info.fields.size());
    }
    case TypeKind::kUnpackedUnion: {
      const auto& info = std::get<UnpackedUnionInfo>(payload);
      return std::format("unpacked_union members={}", info.members.size());
    }
    case TypeKind::kDynamicArray: {
      const auto& info = std::get<DynamicArrayInfo>(payload);
      return std::format(
          "dynamic_array element=type#{}", info.element_type.value);
    }
    case TypeKind::kQueue: {
      const auto& info = std::get<QueueInfo>(payload);
      return std::format(
          "queue element=type#{} max={}", info.element_type.value,
          info.max_bound);
    }
    case TypeKind::kAssociativeArray: {
      const auto& info = std::get<AssociativeArrayInfo>(payload);
      return std::format(
          "assoc_array element=type#{} key=type#{}", info.element_type.value,
          info.key_type.value);
    }
    case TypeKind::kEnum: {
      const auto& info = std::get<EnumInfo>(payload);
      return std::format(
          "enum base=type#{} members={}", info.base_type.value,
          info.members.size());
    }
    default:
      return ToString(kind);
  }
}

}  // namespace

auto TypeArena::Intern(TypeKind kind, TypePayload payload) -> TypeId {
  TypeKey key{.kind = kind, .payload = payload};
  auto it = map_.find(key);
  if (it != map_.end()) {
    return it->second;
  }

  if (mutation_state_ == MutationState::kFrozen) {
    throw common::InternalError(
        "TypeArena::Intern",
        std::format(
            "post-freeze type miss: {}", DescribePayload(kind, payload)));
  }

  TypeId id{static_cast<uint32_t>(types_.size())};
  types_.emplace_back();
  types_.back().kind_ = kind;
  types_.back().payload_ = std::move(payload);
  map_[key] = id;
  return id;
}

auto TypeArena::operator[](TypeId id) const -> const Type& {
  return types_[id.value];
}

auto TypeArena::InternField(TypeId type, uint32_t ordinal, FieldInfo info)
    -> FieldId {
  auto key = std::make_pair(type, ordinal);
  auto it = field_id_map_.find(key);
  if (it != field_id_map_.end()) {
    return it->second;
  }

  if (mutation_state_ == MutationState::kFrozen) {
    throw common::InternalError(
        "TypeArena::InternField",
        std::format(
            "post-freeze field miss: type={}, ordinal={}", type.value,
            ordinal));
  }

  FieldId id{static_cast<uint32_t>(fields_.size())};
  fields_.push_back(std::move(info));
  field_id_map_[key] = id;
  return id;
}

void TypeArena::Freeze() {
  if (mutation_state_ == MutationState::kFrozen) {
    throw common::InternalError("TypeArena::Freeze", "arena is already frozen");
  }
  mutation_state_ = MutationState::kFrozen;
}

auto TypeArena::GetFieldId(TypeId type, uint32_t ordinal) const -> FieldId {
  auto it = field_id_map_.find(std::make_pair(type, ordinal));
  if (it == field_id_map_.end()) {
    return kInvalidFieldId;
  }
  return it->second;
}

auto TypeArena::GetField(FieldId id) const -> const FieldInfo& {
  return fields_[id.value];
}

}  // namespace lyra
