#pragma once

#include <cstddef>
#include <cstdint>
#include <format>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"

namespace lyra::base {

// A flat, append-only pool of `T` indexed by a typed id `Id`. `Id` is a struct
// carrying a single `std::uint32_t value` (its position in the pool). One id is
// minted per append, in insertion order, and is the only durable handle: it
// stays valid for the life of the arena, lookup is by id alone, and one element
// references another by id, never by pointer.
//
// A node is built before it is appended and is never edited in place. This
// value immutability is load-bearing: an appended node's value is final, so
// other nodes reference it by id and a later shard caches it without
// coordination.
//
// Value immutability is not address stability. `Get` returns a transient view
// into relocatable backing storage; any `Add` may relocate that storage and so
// invalidates every reference, pointer, and iterator a prior `Get` returned. To
// use a fact past an `Add`, project it to a value -- an `Id`, a kind, a copied
// field -- before the `Add`; never hold a `Get` reference across one.
template <typename T, typename Id>
class Arena {
 public:
  [[nodiscard]] auto Get(Id id) const -> const T& {
    // A typed id that overruns the pool is a compiler-invariant violation -- it
    // was minted for a different arena or carried past its arena's lifetime --
    // so report the id and pool size rather than let the bare bounds check
    // throw a contextless std::out_of_range.
    if (id.value >= items_.size()) {
      throw InternalError(
          std::format(
              "Arena::Get: id {} is out of range; the arena holds {} elements",
              id.value, items_.size()));
    }
    return items_[id.value];
  }

  auto Add(T value) -> Id {
    const Id id{static_cast<std::uint32_t>(items_.size())};
    items_.push_back(std::move(value));
    return id;
  }

  [[nodiscard]] auto size() const -> std::size_t {
    return items_.size();
  }

  [[nodiscard]] auto empty() const -> bool {
    return items_.empty();
  }

  [[nodiscard]] auto begin() const {
    return items_.begin();
  }

  [[nodiscard]] auto end() const {
    return items_.end();
  }

 private:
  std::vector<T> items_;
};

}  // namespace lyra::base
