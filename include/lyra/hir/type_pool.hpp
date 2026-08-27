#pragma once

#include <cstddef>
#include <unordered_map>

#include "lyra/base/arena.hpp"
#include "lyra/base/id_range.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/type_id.hpp"

namespace lyra::hir {

// How a pool spreads the types it holds. It reads the type's arm and the
// identities that arm names rather than walking a member list, because equality
// decides the answer and a hash only has to separate the types one unit
// actually holds.
struct TypeDataHash {
  auto operator()(const TypeData& data) const -> std::size_t;
};

// The types one compilation unit names. A type's identity here is its
// structure, so two declarations spelling the same type are one entry however
// each was reached -- read off the frontend, composed by the lowering, or taken
// out of another unit's signature. That is what makes a `TypeId` mean a type
// rather than a place where one happened to be written, and it is what keeps a
// type arriving from outside from becoming a second copy of one already here.
//
// Identity is structural and not the frontend's: a pool outlives the frontend
// object that fed it and belongs to one unit alone, so nothing in it may rest
// on a table shared with another unit.
class TypePool {
 public:
  // The identity this pool knows `data` by, adding it if this is the first
  // time the pool has been asked for that type.
  auto Intern(TypeData data) -> TypeId;

  [[nodiscard]] auto Get(TypeId id) const -> const Type& {
    return types_.Get(id);
  }

  [[nodiscard]] auto size() const -> std::size_t {
    return types_.size();
  }

  [[nodiscard]] auto Ids() const -> base::IdRange<TypeId> {
    return types_.Ids();
  }

 private:
  base::Arena<Type, TypeId> types_;
  std::unordered_map<TypeData, TypeId, TypeDataHash> interned_;
};

}  // namespace lyra::hir
