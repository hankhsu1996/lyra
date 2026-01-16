#pragma once

#include <variant>

#include "lyra/mir/effect.hpp"
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

// Effect: side-effect operation with no result value
struct Effect {
  EffectOp op;
};

// An instruction that does not affect control flow.
// - Assign and Compute write to a Place
// - Effect produces side effects but no value
using Instruction = std::variant<Assign, Compute, Effect>;

}  // namespace lyra::mir
