#include "lyra/common/type_arena.hpp"

#include <cstdint>
#include <utility>

#include "lyra/common/type.hpp"

namespace lyra {

auto TypeArena::Intern(TypeKind kind, TypePayload payload) -> TypeId {
  TypeKey key{.kind = kind, .payload = payload};
  auto it = map_.find(key);
  if (it != map_.end()) {
    return it->second;
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
  FieldId id{static_cast<uint32_t>(fields_.size())};
  fields_.push_back(std::move(info));
  field_id_map_[key] = id;
  return id;
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
