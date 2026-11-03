#pragma once

#include <cstddef>
#include <cstdint>
#include <vector>

namespace lyra::mir {

// The bits of an integral constant, and nothing else. How wide the value is,
// whether it is signed, and whether it has an unknown plane at all are the
// type's to state, and every expression carries one, which is where a consumer
// reads them.
//
// Word layout is LSB-first and 4-state encoding is (v=0,s=0)=0, (v=1,s=0)=1,
// (v=0,s=1)=Z, (v=1,s=1)=X. Nothing else is promised here: this is what a
// caller hands over, and a caller with one word in hand for a narrower type
// writes one word. What the words have to look like to be a constant of a unit
// is on the value a unit holds, which is where it is established.
struct IntegralConstant {
  std::vector<std::uint64_t> value_words;
  std::vector<std::uint64_t> state_words;

  auto operator==(const IntegralConstant&) const -> bool = default;
};

// Folds both planes into `seed`, for a key that holds a constant among its
// parts.
void HashIntegralConstant(std::size_t& seed, const IntegralConstant& value);

}  // namespace lyra::mir
