#include "lyra/common/constant_arena.hpp"

#include <utility>

namespace lyra {

auto ConstantArena::Intern(TypeId type, ConstantValue value) -> ConstId {
  Constant key{.type = type, .value = value};
  auto it = map_.find(key);
  if (it != map_.end()) {
    return it->second;
  }

  ConstId id{static_cast<uint32_t>(constants_.size())};
  constants_.push_back(Constant{.type = type, .value = std::move(value)});
  map_[key] = id;
  return id;
}

auto ConstantArena::operator[](ConstId id) const -> const Constant& {
  return constants_[id.value];
}

}  // namespace lyra
