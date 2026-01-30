#pragma once

#include <optional>
#include <variant>
#include <vector>

#include "lyra/common/origin_id.hpp"
#include "lyra/mir/builtin.hpp"
#include "lyra/mir/call.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/rhs.hpp"

namespace lyra::mir {

// Assign: immediate write (dest := rhs).
// RightHandSide can be Operand (simple value) or Rvalue (computation).
struct Assign {
  PlaceId dest;
  RightHandSide rhs;
};

// GuardedAssign: conditional write (if guard then dest := rhs).
// Semantics: rhs is always evaluated; only the write is gated by guard.
// For short-circuit semantics (rhs has side effects), use control flow.
struct GuardedAssign {
  PlaceId dest;
  RightHandSide rhs;
  Operand guard;  // 1-bit 2-state bool
};

// Effect: side-effect operation with no result value
struct Effect {
  EffectOp op;
};

// DeferredAssign: scheduled write (dest := rhs in NBA region).
// Semantics: enqueue write for commit in a later region (NBA semantics).
struct DeferredAssign {
  PlaceId dest;
  RightHandSide rhs;
};

// Call: unified invocation for user functions and system TFs.
//
// CONTRACT: Unified call execution model
//
// All Call outputs (return + writebacks) follow the same staging discipline:
// 1. tmp places are MIR-visible and preallocated by PlaceCollector
// 2. Call execution writes results ONLY to tmp places
// 3. After call returns, results are committed:
//    - LLVM: CommitValue(dest, Load(tmp), type, kMove)
//    - Interpreter: StoreToPlace(dest, value_from_tmp)
// 4. No direct "callee writes to dest" - even sret passes tmp pointer
//
// Commit order: ret first (if present), then writebacks in arg_index order.
// Alias policy: overlapping dests fail loud (InternalError) until aliasing
// supported.
//
// This ensures:
// - All Place writes go through lifecycle choke point
// - Managed type semantics (retain/release) correctly applied
// - Design slot notifications happen at the right time
// - No hidden alloca creation during lowering (all temps preallocated)
//
// Invariant: PlaceCollector collects ONLY tmp fields, NOT dest fields.
// dest may reference design slots, hierarchical places, or subplaces that
// must not trigger prologue allocas.
struct Call {
  Callee callee;                          // User function or system TF opcode
  std::vector<Operand> in_args;           // Input arguments (includes InOut
                                          // copy-in)
  std::optional<CallReturn> ret;          // Return value (nullopt = void)
  std::vector<CallWriteback> writebacks;  // Out/InOut parameters (0..N)
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
// DEPRECATED: Being migrated to unified Call with
// SystemTfOpcode::kValuePlusargs. Will be removed once HIR->MIR lowering and
// backends are migrated.
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

// Statement data variant.
using StatementData = std::variant<
    Assign, GuardedAssign, Effect, DeferredAssign, Call, BuiltinCall,
    ValuePlusargs>;

// A statement that does not affect control flow.
// - Assign, GuardedAssign, DeferredAssign write to a Place
// - Effect produces side effects but no value
struct Statement {
  StatementData data;
  common::OriginId origin = common::OriginId::Invalid();
};

}  // namespace lyra::mir
