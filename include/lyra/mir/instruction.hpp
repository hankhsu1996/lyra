#pragma once

#include "lyra/mir/place.hpp"
#include "lyra/mir/rvalue.hpp"

namespace lyra::mir {

// An instruction computes a value and writes it to a Place.
// It does not affect control flow.
struct Instruction {
  Place target;  // destination storage
  Rvalue value;  // computation recipe
};

}  // namespace lyra::mir
