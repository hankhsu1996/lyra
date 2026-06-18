#pragma once

#include <optional>

#include "lyra/runtime/runtime_services.hpp"
#include "lyra/runtime/var.hpp"
#include "lyra/value/associative_array.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::runtime {

// LRM 7.9.4 -- 7.9.7 associative-array traversal. Each function assigns the
// visited key into the caller's index variable and returns 1 when an entry is
// found or 0 when none is (empty array, or no next / prev -- index unchanged).
// slang enforces that the index argument is type-equivalent to the array's
// index type, so the LRM 7.9.8 narrow-argument case (truncate and return -1)
// cannot reach this layer. The write routes through `Ref::Set` so an
// observable index variable fires its LRM 4.3 update event, matching a
// user-defined `ref` argument.

namespace detail {

template <typename K>
auto WriteTraversalKey(RuntimeServices& services, Ref<K> index, const K& key)
    -> value::PackedArray {
  index.Set(services, key);
  return value::PackedArray::Int(1);
}

}  // namespace detail

template <typename K, typename V>
auto AssocFirst(
    RuntimeServices& services, const value::AssociativeArray<K, V>& map,
    Ref<K> index) -> value::PackedArray {
  const std::optional<K> key = map.FirstKey();
  if (!key.has_value()) {
    return value::PackedArray::Int(0);
  }
  return detail::WriteTraversalKey(services, index, *key);
}

template <typename K, typename V>
auto AssocLast(
    RuntimeServices& services, const value::AssociativeArray<K, V>& map,
    Ref<K> index) -> value::PackedArray {
  const std::optional<K> key = map.LastKey();
  if (!key.has_value()) {
    return value::PackedArray::Int(0);
  }
  return detail::WriteTraversalKey(services, index, *key);
}

template <typename K, typename V>
auto AssocNext(
    RuntimeServices& services, const value::AssociativeArray<K, V>& map,
    Ref<K> index) -> value::PackedArray {
  const std::optional<K> key = map.NextKey(index.Get());
  if (!key.has_value()) {
    return value::PackedArray::Int(0);
  }
  return detail::WriteTraversalKey(services, index, *key);
}

template <typename K, typename V>
auto AssocPrev(
    RuntimeServices& services, const value::AssociativeArray<K, V>& map,
    Ref<K> index) -> value::PackedArray {
  const std::optional<K> key = map.PrevKey(index.Get());
  if (!key.has_value()) {
    return value::PackedArray::Int(0);
  }
  return detail::WriteTraversalKey(services, index, *key);
}

}  // namespace lyra::runtime
