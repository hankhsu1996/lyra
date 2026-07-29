#pragma once

#include <cstdint>
#include <vector>

namespace lyra::lir {

// A LIR-owned integral constant value: the bits, and nothing else. How wide the
// value is, whether it is signed, and whether it has an unknown plane at all
// are the type's to state, and the type is on the operand that carries this --
// so a consumer reads them there and the two can never disagree.
//
// Word layout is LSB-first; the top word's unused high bits are zero-masked.
// `value_words` holds one word per 64 bits of the type's width, rounded up, so
// the constant carries its whole value and a consumer hands the planes on as
// they stand. `state_words` is empty for a two-state value, otherwise the same
// length as `value_words` (4-state encoding: value bit plus state bit per
// lane).
struct IntegralConstant {
  std::vector<std::uint64_t> value_words;
  std::vector<std::uint64_t> state_words;
};

}  // namespace lyra::lir
