#pragma once

#include <algorithm>
#include <iterator>
#include <utility>
#include <vector>

namespace lyra::base {

// Whether this build runs under the address sanitizer. GCC states it with a
// macro of its own and clang through a feature test.
#if defined(__SANITIZE_ADDRESS__)
inline constexpr bool kUnderAddressSanitizer = true;
#elif defined(__has_feature)
#if __has_feature(address_sanitizer)
inline constexpr bool kUnderAddressSanitizer = true;
#else
inline constexpr bool kUnderAddressSanitizer = false;
#endif
#else
inline constexpr bool kUnderAddressSanitizer = false;
#endif

// Called by a pool ahead of each append. A pool's storage may move on any
// append, and a reference held across one reads freed storage only on the
// appends where it happened to move, so an ordinary run says nothing about
// whether the code holds one. Under the sanitizer the storage is moved on every
// append instead, with room for exactly the element about to arrive, and a held
// reference is then reported the first time it is read.
template <typename T>
void MoveBeforeAppendUnderSanitizer(std::vector<T>& items) {
  if (!kUnderAddressSanitizer) return;
  std::vector<T> moved;
  moved.reserve(items.size() + 1);
  std::ranges::move(items, std::back_inserter(moved));
  items = std::move(moved);
}

}  // namespace lyra::base
