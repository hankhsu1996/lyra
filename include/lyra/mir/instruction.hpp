#pragma once

#include <variant>

#include "lyra/common/unsupported_error.hpp"
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

// GuardedAssign: conditional write with OOB safety.
// Semantics: if (validity) Assign(target, source); else no-op
// The source is always evaluated; only the write is guarded.
// For short-circuit semantics (source has side effects), use control flow.
struct GuardedAssign {
  PlaceId target;
  Operand source;
  Operand validity;  // 1-bit 2-state bool
};

// Effect: side-effect operation with no result value
struct Effect {
  EffectOp op;
};

// Instruction data variant.
using InstructionData = std::variant<Assign, Compute, GuardedAssign, Effect>;

// An instruction that does not affect control flow.
// - Assign, Compute, and GuardedAssign write to a Place
// - Effect produces side effects but no value
struct Instruction {
  InstructionData data;
  common::OriginId origin = common::OriginId::Invalid();
};

}  // namespace lyra::mir
