#pragma once

#include <cstddef>
#include <memory>
#include <new>
#include <utility>

namespace lyra::value {

// The most bytes a value may be and still be kept where it is used rather than
// in storage of its own. A frame is taken from a stack whose size no design
// chose, so a frame keeping a value whose size followed its declared width
// would follow that width too; a larger value is kept apart, through a box the
// program states, and a holder keeps one apart likewise.
inline constexpr std::size_t kMaxInlineBytes = 1024;

// A `T` built at `storage`, which holds nothing yet, from what `make` answers,
// so the value is made where it is kept and never first in the frame of
// whoever builds it.
template <typename T, typename Make>
void BuildFrom(void* storage, Make&& make) {
  ::new (storage) T(std::forward<Make>(make)());
}

// `target` ended and built again from what `make` answers, made where `target`
// lies.
template <typename T, typename Make>
void RebuildFrom(T& target, Make&& make) {
  std::destroy_at(&target);
  BuildFrom<T>(&target, std::forward<Make>(make));
}

}  // namespace lyra::value
