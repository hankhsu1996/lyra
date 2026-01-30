#pragma once

#include <optional>
#include <variant>
#include <vector>

#include "lyra/common/origin_id.hpp"
#include "lyra/mir/builtin.hpp"
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

// NonBlockingAssign: deferred data movement (NBA region).
// Semantics: schedule target <= source for commit in the NBA region.
struct NonBlockingAssign {
  PlaceId target;
  Operand source;
};

// Call: user function invocation
//
// `dest` is the place written by this call (ABI-agnostic):
// - If ABI returns in registers -> result is written to `dest`
// - If ABI is sret/out-param -> callee writes to `dest` directly
// - If `dest == nullopt` -> call is for side effects (void return)
//
// The lowering layer determines the actual calling convention based on
// RequiresSret(return_type). MIR doesn't distinguish - it just specifies
// where the result goes.
struct Call {
  std::optional<PlaceId> dest;
  FunctionId callee;
  std::vector<Operand> args;
};

// BuiltinCall: container-mutating builtin operations
//
// Covers push/pop/delete/insert - operations that mutate a receiver container.
// Pop operations have dest (return popped value), others have nullopt.
//
// INVARIANT: receiver is always an assignable lvalue (PlaceId).
// Container methods are only valid on assignable containers in SystemVerilog.
// HIR->MIR lowering asserts this; temporary receivers are not allowed.
struct BuiltinCall {
  std::optional<PlaceId> dest;
  BuiltinMethod method;
  PlaceId receiver;
  std::vector<Operand> args;
};

// Instruction data variant.
using InstructionData = std::variant<
    Assign, Compute, GuardedAssign, Effect, NonBlockingAssign, Call,
    BuiltinCall>;

// An instruction that does not affect control flow.
// - Assign, Compute, and GuardedAssign write to a Place
// - Effect produces side effects but no value
struct Instruction {
  InstructionData data;
  common::OriginId origin = common::OriginId::Invalid();
};

}  // namespace lyra::mir
