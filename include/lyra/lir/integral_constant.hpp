#pragma once

#include <cstdint>
#include <vector>

#include "lyra/lir/type_id.hpp"

namespace lyra::lir {

// A LIR-owned integral constant value: the bits, and nothing else. How wide the
// value is, whether it is signed, and whether it has an unknown plane at all
// are the type's to state, and whatever carries this states the type beside
// it, which is where a consumer reads them.
//
// Word layout is LSB-first; the top word's unused high bits are zero-masked.
// `value_words` holds one word per 64 bits of the type's width, rounded up, so
// the constant carries its whole value and a consumer hands the planes on as
// they stand. `state_words` holds the unknown plane the same way (4-state
// encoding: value bit plus state bit per lane). It is empty for a two-state
// value, and an empty one beside a four-state type is a plane with no bit set.
struct IntegralConstant {
  std::vector<std::uint64_t> value_words;
  std::vector<std::uint64_t> state_words;
};

// One constant a unit holds: the integral type it is a value of, and its bits.
struct IntegralConstantDecl {
  TypeId type;
  IntegralConstant value;
};

}  // namespace lyra::lir
