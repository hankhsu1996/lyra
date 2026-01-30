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

  auto InternField(TypeId type, uint32_t ordinal, FieldInfo info) -> FieldId;

  [[nodiscard]] auto GetFieldId(TypeId type, uint32_t ordinal) const -> FieldId;

  [[nodiscard]] auto GetField(FieldId id) const -> const FieldInfo&;

 private:
  std::vector<Type> types_;
  absl::flat_hash_map<TypeKey, TypeId, TypeKeyHash> map_;
  std::vector<FieldInfo> fields_;
  absl::flat_hash_map<std::pair<TypeId, uint32_t>, FieldId> field_id_map_;
};

}  // namespace lyra
