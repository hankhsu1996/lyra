#pragma once

#include <cstdint>
#include <optional>
#include <variant>
#include <vector>

#include "lyra/common/math_fn.hpp"
#include "lyra/common/runtime_query_kind.hpp"
#include "lyra/common/system_tf.hpp"
#include "lyra/common/type.hpp"
#include "lyra/mir/builtin.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/operator.hpp"

namespace lyra::mir {

// Per-kind info structs - each Rvalue kind has its own info type
// The kind is implicit in which variant alternative is active

struct UnaryRvalueInfo {
  UnaryOp op;
};

struct BinaryRvalueInfo {
  BinaryOp op;
};

struct CastRvalueInfo {
  TypeId source_type;
  TypeId target_type;
};

struct BitCastRvalueInfo {
  TypeId source_type;
  TypeId target_type;
};

struct AggregateRvalueInfo {
  TypeId result_type;  // The aggregate type being constructed (struct or array)
};

// BuiltinCallRvalueInfo for builtin method calls (e.g., arr.size(),
// q.pop_back()).
// - result_type: Required for kNewArray (element type can't be inferred)
// - receiver: For pop methods that both return a value and mutate the queue
// - enum_type: For enum methods (required to look up EnumInfo)
struct BuiltinCallRvalueInfo {
  BuiltinMethod method;
  TypeId result_type;
  std::optional<PlaceId> receiver;
  std::optional<TypeId> enum_type;  // For kEnumNext, kEnumPrev, kEnumName
};

// IndexValidity: computes "this index is a valid access" predicate.
// Returns 1-bit 2-state bool: (lower <= index <= upper) && is_known(index)
// Bounds are logical (always lower <= upper); direction handling happens
// during lowering when computing the bit offset.
struct IndexValidityRvalueInfo {
  int64_t lower_bound;
  int64_t upper_bound;
  bool check_known;  // true for 4-state indices
  // operands[0] = index
};

// GuardedUse: conditionally read from a place with OOB safety.
// Semantics: validity ? Use(place) : oob_default
// This is the one Rvalue that explicitly names a Place rather than taking
// it as an Operand. We can't express "conditionally read" with Use(place)
// alone because the guarding is intrinsic to the read operation.
struct GuardedUseRvalueInfo {
  PlaceId place;
  TypeId result_type;  // Determines OOB default (X for 4-state, 0 for 2-state)
  // operands[0] = validity predicate (1-bit 2-state bool)
};

// Concatenation: bit-concatenate operands MSB to LSB.
// Result type determines width and 2-state vs 4-state.
struct ConcatRvalueInfo {
  TypeId result_type;
};

// Replication: replicate single operand N times.
// operands[0] = element to replicate
// Result type determines width and 2-state vs 4-state.
struct ReplicateRvalueInfo {
  TypeId result_type;
  uint32_t count;
};

// SFormat: produces a RuntimeString from format ops or runtime args.
// Operand layout:
// - Compile-time path (ops non-empty): FormatOp.value has embedded Operands.
//   rv.operands is empty.
// - Runtime format path (has_runtime_format=true, ops empty):
//   rv.operands[0] is format string, rv.operands[1..] are value args.
// - Auto-format path (has_runtime_format=false, ops empty):
//   all rv.operands are value args.
struct SFormatRvalueInfo {
  std::vector<FormatOp> ops;
  FormatKind default_format = FormatKind::kDecimal;
  bool has_runtime_format = false;
};

// TestPlusargsRvalueInfo: $test$plusargs system function (pure, no side
// effects) Returns 1 if a plusarg matching the query prefix exists, 0
// otherwise. Query is stored as TypedOperand for packed-to-string coercion.
struct TestPlusargsRvalueInfo {
  TypedOperand query;  // Query string (typed for packed-to-string coercion)
};

// SystemTfRvalueInfo: Generic rvalue-producing system TFs.
// Covers simple system TFs where payload is just opcode + operands.
// For opcodes that need string coercion (kFopen), use typed_operands.
// Other opcodes continue using Rvalue::operands.
struct SystemTfRvalueInfo {
  SystemTfOpcode opcode;
  std::vector<TypedOperand> typed_operands;  // For kFopen: filename [, mode]
};

// Engine-state queries ($time, $stime, $realtime).
// No operands - reads from engine pointer at runtime.
struct RuntimeQueryRvalueInfo {
  RuntimeQueryKind kind;
};

// MathCallRvalueInfo: IEEE 1800 20.8 math function call.
// Operands from Rvalue::operands (validated: size == GetMathFnArity(fn)).
struct MathCallRvalueInfo {
  MathFn fn;
};

// Variant of all info types - determines Rvalue kind implicitly
using RvalueInfo = std::variant<
    UnaryRvalueInfo, BinaryRvalueInfo, CastRvalueInfo, BitCastRvalueInfo,
    AggregateRvalueInfo, BuiltinCallRvalueInfo, IndexValidityRvalueInfo,
    GuardedUseRvalueInfo, ConcatRvalueInfo, ReplicateRvalueInfo,
    SFormatRvalueInfo, TestPlusargsRvalueInfo, RuntimeQueryRvalueInfo,
    MathCallRvalueInfo, SystemTfRvalueInfo>;

struct Rvalue {
  std::vector<Operand> operands;
  RvalueInfo info;
};

// Helper to get Rvalue kind name from info variant
inline auto GetRvalueKind(const RvalueInfo& info) -> const char* {
  return std::visit(
      [](const auto& i) -> const char* {
        using T = std::decay_t<decltype(i)>;
        if constexpr (std::is_same_v<T, UnaryRvalueInfo>) {
          return "unary";
        } else if constexpr (std::is_same_v<T, BinaryRvalueInfo>) {
          return "binary";
        } else if constexpr (std::is_same_v<T, CastRvalueInfo>) {
          return "cast";
        } else if constexpr (std::is_same_v<T, BitCastRvalueInfo>) {
          return "bitcast";
        } else if constexpr (std::is_same_v<T, AggregateRvalueInfo>) {
          return "aggregate";
        } else if constexpr (std::is_same_v<T, BuiltinCallRvalueInfo>) {
          return "builtin";
        } else if constexpr (std::is_same_v<T, IndexValidityRvalueInfo>) {
          return "index_validity";
        } else if constexpr (std::is_same_v<T, GuardedUseRvalueInfo>) {
          return "guarded_use";
        } else if constexpr (std::is_same_v<T, ConcatRvalueInfo>) {
          return "concat";
        } else if constexpr (std::is_same_v<T, ReplicateRvalueInfo>) {
          return "replicate";
        } else if constexpr (std::is_same_v<T, SFormatRvalueInfo>) {
          return "sformat";
        } else if constexpr (std::is_same_v<T, TestPlusargsRvalueInfo>) {
          return "test_plusargs";
        } else if constexpr (std::is_same_v<T, RuntimeQueryRvalueInfo>) {
          return "runtime_query";
        } else if constexpr (std::is_same_v<T, MathCallRvalueInfo>) {
          return "math_call";
        } else if constexpr (std::is_same_v<T, SystemTfRvalueInfo>) {
          return "system_tf";
        } else {
          static_assert(false, "unhandled RvalueInfo kind");
        }
      },
      info);
}

}  // namespace lyra::mir
