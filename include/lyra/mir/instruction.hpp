#pragma once

#include <optional>
#include <variant>
#include <vector>

#include "lyra/common/origin_id.hpp"
#include "lyra/mir/builtin.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/rhs.hpp"

namespace lyra::mir {

// Assign: unified data movement and computation (Place <- RightHandSide).
// RightHandSide can be either Operand (simple value) or Rvalue (computation).
struct Assign {
  PlaceId target;
  RightHandSide source;
};

// GuardedStore: conditional write with OOB safety.
// Semantics: if (validity) Assign(target, source); else no-op
// The source is always evaluated; only the write is guarded.
// For short-circuit semantics (source has side effects), use control flow.
struct GuardedStore {
  PlaceId target;
  RightHandSide source;
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

// ValuePlusargs: $value$plusargs system function (has side effects + result)
//
// Semantics: parse format string from plusargs, write parsed value to output,
// store success (1/0) to dest.
// Unlike $test$plusargs (pure rvalue), this has side effects (writes to
// output).
struct ValuePlusargs {
  PlaceId dest;        // Where to store success boolean (1 or 0)
  PlaceId output;      // Place to write parsed value
  TypeId output_type;  // Type of output for runtime dispatch (string/int)
  Operand query;       // Format string operand
};

// Instruction data variant.
using InstructionData = std::variant<
    Assign, GuardedStore, Effect, NonBlockingAssign, Call, BuiltinCall,
    ValuePlusargs>;

// An instruction that does not affect control flow.
// - Assign and GuardedStore write to a Place
// - Effect produces side effects but no value
struct Instruction {
  InstructionData data;
  common::OriginId origin = common::OriginId::Invalid();
};

}  // namespace lyra::mir
