#pragma once

#include <cstdint>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "absl/hash/hash.h"
#include "lyra/common/type.hpp"

// TypeArena: interning arena for all compile-global types and fields.
//
// Mutation contract:
//   - While mutable (initial state), Intern() and InternField() may create
//     new entries or return existing ones (dedup).
//   - After Freeze(), only lookups (hits) are allowed. A miss after freeze
//     is an InternalError -- it means Phase 0 seeding failed to cover a type
//     that Phase 1 body lowering needs.
//   - Freeze() is a one-way transition. Calling it twice is an InternalError.

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

  void Freeze();

  [[nodiscard]] auto IsFrozen() const -> bool {
    return mutation_state_ == MutationState::kFrozen;
  }

  [[nodiscard]] auto TypeCount() const -> uint32_t {
    return static_cast<uint32_t>(types_.size());
  }

  [[nodiscard]] auto FieldCount() const -> uint32_t {
    return static_cast<uint32_t>(fields_.size());
  }

 private:
  enum class MutationState { kMutable, kFrozen };

  MutationState mutation_state_ = MutationState::kMutable;
  std::vector<Type> types_;
  absl::flat_hash_map<TypeKey, TypeId, TypeKeyHash> map_;
  std::vector<FieldInfo> fields_;
  absl::flat_hash_map<std::pair<TypeId, uint32_t>, FieldId> field_id_map_;
};

}  // namespace lyra
