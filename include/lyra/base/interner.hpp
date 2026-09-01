#pragma once

#include <cstddef>
#include <functional>
#include <unordered_map>
#include <utility>

#include "lyra/base/arena.hpp"
#include "lyra/base/id_range.hpp"

namespace lyra::base {

// A pool of `T` indexed by a typed id `Id`, where the identity is derived from
// the value rather than conferred on it: interning the same content twice
// answers with the same id, so an id comparison is a content comparison. This
// serves entities that are structural -- two of them that look alike are the
// same entity -- which is what makes folding them onto one identity correct
// rather than lossy.
//
// `Hash` and `Equal` are the definition of that sameness, and they are the
// pool's whole content-facing surface: whatever they ignore is not part of the
// entity's identity. `Equal` defaults to `T`'s own equality, which is right
// wherever every field a `T` carries is part of what it means.
//
// An identity here cannot precede its value, and that is what the derivation
// means rather than a limit on it: there is no content to derive from until the
// content exists. An entity that must be referable before it is complete is
// nominal, not structural -- its identity is conferred, and `base::Registry` is
// the pool for it. A recursive structure is reached through such an entity,
// which is why interning needs no reserve-then-fill step.
//
// Interning names a value; it does not change the program. A request for an
// entity already interned is indistinguishable from the first request, so the
// storage and the index are a memo kept behind an observationally pure surface
// and a `const` pool still answers.
//
// `base::Arena` is the counterpart that confers instead: it mints one id per
// append, so two equal values there are two entities. Reach for this one only
// where two equal values must be one.
template <
    typename T, typename Id, typename Hash, typename Equal = std::equal_to<T>>
class Interner {
 public:
  // The identity this pool knows `value` by, adding it if this is the first
  // time the pool has been asked for that content.
  auto Intern(T value) const -> Id {
    if (const auto it = index_.find(value); it != index_.end()) {
      return it->second;
    }
    const Id id = storage_.Add(value);
    index_.emplace(std::move(value), id);
    return id;
  }

  // Keeps the arena's transient-view contract: the `Id` is the durable handle,
  // and a reference this returns must not be held across a later `Intern`.
  [[nodiscard]] auto Get(Id id) const -> const T& {
    return storage_.Get(id);
  }

  [[nodiscard]] auto size() const -> std::size_t {
    return storage_.size();
  }

  [[nodiscard]] auto empty() const -> bool {
    return storage_.empty();
  }

  // The identities this pool has handed out, for a consumer that walks what it
  // holds. Reading them from here is what keeps a walk from rebuilding an id
  // out of its own loop counter.
  [[nodiscard]] auto Ids() const -> IdRange<Id> {
    return storage_.Ids();
  }

  [[nodiscard]] auto begin() const {
    return storage_.begin();
  }

  [[nodiscard]] auto end() const {
    return storage_.end();
  }

 private:
  mutable Arena<T, Id> storage_;
  mutable std::unordered_map<T, Id, Hash, Equal> index_;
};

}  // namespace lyra::base
