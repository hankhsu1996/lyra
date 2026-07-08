#pragma once

#include <cstdint>

namespace lyra::value {

// The source-level shape of a range selector, carried raw from lowering to the
// selected value. The value resolves the coordinate window from this plus the
// two raw operands and its own declared range: a constant range `[l:r]` passes
// `(l, r)`; an indexed part-select `[base +: w]` / `[base -: w]` passes
// `(base, w)` with the direction here. No count, offset, or ordinal is computed
// at lowering.
enum class SliceForm : std::int64_t {
  kConstant = 0,
  kIndexedUp = 1,
  kIndexedDown = 2,
};

}  // namespace lyra::value
