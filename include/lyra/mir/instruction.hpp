#pragma once

#include <variant>

#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/rvalue.hpp"

namespace lyra::mir {

// Assign: data movement (Place <- Operand)
struct Assign {
  PlaceId target;
  Operand source;
};

// Compute: computation (Place <- Rvalue)
struct Compute {
  PlaceId target;
  Rvalue value;
};

// An instruction writes to a Place. It does not affect control flow.
using Instruction = std::variant<Assign, Compute>;

}  // namespace lyra::mir
