#pragma once

#include <cstdint>
#include <vector>

#include "lyra/mir/type.hpp"

namespace lyra::mir {

enum class IntegralStateKind : std::uint8_t {
  kTwoState,
  kFourState,
};

// Word layout is LSB-first; top word's unused high bits are zero-masked.
// `width` is at least one bit and `value_words` holds exactly one word per 64
// bits of it, rounded up, so a constant always carries its whole value and a
// consumer never sizes the planes itself. state_words is empty for 2-state,
// otherwise same length as value_words.
// 4-state encoding: (v=0,s=0)=0, (v=1,s=0)=1, (v=0,s=1)=Z, (v=1,s=1)=X.
struct IntegralConstant {
  std::vector<std::uint64_t> value_words;
  std::vector<std::uint64_t> state_words;
  std::uint32_t width = 0;
  Signedness signedness = Signedness::kUnsigned;
  IntegralStateKind state_kind = IntegralStateKind::kTwoState;
};

}  // namespace lyra::mir
