#pragma once

#include <cstdint>
#include <vector>

#include "lyra/lir/type.hpp"

namespace lyra::lir {

enum class IntegralStateKind : std::uint8_t { kTwoState, kFourState };

// A LIR-owned integral constant value. Word layout is LSB-first; the top word's
// unused high bits are zero-masked. `state_words` is empty for two-state,
// otherwise the same length as `value_words` (4-state encoding: value bit plus
// state bit per lane). Translated from the producing IR at the boundary; LIR
// carries no upstream constant type.
struct IntegralConstant {
  std::vector<std::uint64_t> value_words;
  std::vector<std::uint64_t> state_words;
  std::uint32_t width = 0;
  Signedness signedness = Signedness::kUnsigned;
  IntegralStateKind state_kind = IntegralStateKind::kTwoState;
};

}  // namespace lyra::lir
