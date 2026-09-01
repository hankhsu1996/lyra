#pragma once

#include <cstdint>
#include <vector>

namespace lyra::mir {

// The bits of an integral constant, and nothing else. How wide the value is,
// whether it is signed, and whether it has an unknown plane at all are the
// type's to state, and every expression carries one -- so a consumer reads them
// there and the two can never disagree.
//
// Word layout is LSB-first; the top word's unused high bits are zero-masked.
// `value_words` holds one word per 64 bits of the type's width, rounded up, so
// the constant carries its whole value and a consumer never sizes the planes
// itself. `state_words` is empty for a two-state value, otherwise the same
// length as `value_words`.
// 4-state encoding: (v=0,s=0)=0, (v=1,s=0)=1, (v=0,s=1)=Z, (v=1,s=1)=X.
struct IntegralConstant {
  std::vector<std::uint64_t> value_words;
  std::vector<std::uint64_t> state_words;
};

}  // namespace lyra::mir
