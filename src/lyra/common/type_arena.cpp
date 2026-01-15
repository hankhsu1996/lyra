#include "lyra/common/type_arena.hpp"

#include <utility>

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

}  // namespace lyra
