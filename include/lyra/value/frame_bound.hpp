#pragma once

#include <cstddef>
#include <memory>
#include <utility>

#include "lyra/support/layout.hpp"

namespace lyra::value {

// The most bytes a value may be and still be kept where it is used rather than
// in storage of its own. A frame is taken from a stack whose size no design
// chose, so a frame keeping a value whose size followed its declared width
// would follow that width too; a larger value is kept apart, through a box the
// program states, and a holder keeps one apart likewise.
inline constexpr std::size_t kMaxInlineBytes = 1024;

// The most bytes the values of one body -- its locals, its variables' cells and
// what its expressions make -- may take in its frame together, each within the
// bound above. A frame keeping every one would follow how many the design
// writes, each up to that bound, so past this one a value is held through a box
// as a value too large is, and the frame keeps its address.
inline constexpr std::size_t kMaxFrameBytes = 16 * kMaxInlineBytes;

// Whether a value laid out as `held` lies inline in what holds it rather than
// in storage of its own: it fits the size a value may be and still lie where it
// is used, and the alignment every holder's own storage serves. The one rule
// every holder of a value -- one compiled with its type, one told its layout
// where it runs -- and the code generator laying a holder out read.
[[nodiscard]] constexpr auto LiesInline(support::Layout held) -> bool {
  return held.size <= kMaxInlineBytes && held.align <= alignof(void*);
}

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
