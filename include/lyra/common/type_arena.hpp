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

}  // namespace lyra
