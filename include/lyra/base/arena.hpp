#pragma once

#include <cstddef>
#include <cstdint>
#include <utility>
#include <vector>

namespace lyra::base {

// A flat, append-only pool of `T` indexed by a typed id `Id`. `Id` is a struct
// carrying a single `std::uint32_t value` (its position in the pool). One id is
// minted per append, in insertion order, and stays valid for the life of the
// arena. Lookup is by id only -- there is no raw integer index -- so the typed
// id is the one handle a consumer holds. There is no mutable lookup: a node is
// built before it is appended and is never edited in place.
template <typename T, typename Id>
class Arena {
 public:
  [[nodiscard]] auto Get(Id id) const -> const T& {
    return items_.at(id.value);
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
