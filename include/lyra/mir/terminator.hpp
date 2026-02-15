#pragma once

#include <cstdint>
#include <optional>
#include <variant>
#include <vector>

#include "lyra/common/bit_target_mapping.hpp"
#include "lyra/common/edge_kind.hpp"
#include "lyra/common/origin_id.hpp"
#include "lyra/common/termination_kind.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"

namespace lyra::mir {

// Unconditional jump to a single target.
struct Jump {
  BasicBlockId target;
  std::vector<Operand> args;  // Values for target.params (edge args for SSA)
};

// Conditional branch based on a boolean condition.
struct Branch {
  Operand condition;  // Can be Use (place) or UseTemp (value)
  BasicBlockId then_target;
  std::vector<Operand> then_args;  // Values for then_target.params
  BasicBlockId else_target;
  std::vector<Operand> else_args;  // Values for else_target.params
};

// Multi-way switch based on selector value.
struct Switch {
  Operand selector;                   // Can be Use (place) or UseTemp (value)
  std::vector<BasicBlockId> targets;  // Index targets + default (last)
};

// Qualifier for QualifiedDispatch terminator (unique/unique0).
// Priority uses regular branch cascade, not QualifiedDispatch.
enum class DispatchQualifier : uint8_t {
  kUnique,   // Warn on overlap AND no-match (unless has_else)
  kUnique0,  // Warn on overlap only
};

// Source statement kind for QualifiedDispatch (affects warning messages).
enum class DispatchStatementKind : uint8_t {
  kIf,    // "unique if" / "unique0 if"
  kCase,  // "unique case" / "unique0 case"
};

// Multi-condition dispatch with unique/unique0 semantics.
// All conditions are pre-evaluated before the dispatch decision.
// Semantic contract:
// 1. Count how many conditions are true
// 2. If count > 1: emit overlap warning
// 3. If count == 0 AND !has_else AND qualifier == kUnique: emit no-match
// warning
// 4. Dispatch to first true condition's target (or else target if none true)
struct QualifiedDispatch {
  DispatchQualifier qualifier;
  DispatchStatementKind statement_kind;
  std::vector<Operand> conditions;    // Pre-evaluated condition results
  std::vector<BasicBlockId> targets;  // One per condition + else (last)
  bool has_else;                      // Suppresses no-match warning for kUnique
};

// Time delay suspension (requires scheduler).
struct Delay {
  uint64_t ticks = 0;        // Canonical delay ticks (from HIR)
  BasicBlockId resume = {};  // Block to resume after delay
};

// Late-bound index metadata for dynamic-index edge triggers.
// Carries the index variable location and affine mapping from SV index to
// storage bit. Used by LLVM codegen to emit dynamic byte_offset computation
// and by runtime to create rebind subscriptions.
struct LateBoundIndex {
  SlotId index_slot;
  uint32_t index_byte_offset = 0;
  uint32_t index_byte_size = 0;
  uint32_t index_bit_width = 0;  // Actual SV bit width (1-64)
  bool index_is_signed = false;  // SV type signedness
  runtime::BitTargetMapping mapping;
};

// A single trigger in a Wait terminator.
struct WaitTrigger {
  SlotId signal;
  common::EdgeKind edge = common::EdgeKind::kAnyChange;
  // Optional place reference for sub-slot observation narrowing.
  // Set by sensitivity analysis when the observed sub-range is statically known
  // (e.g., struct field, constant array index). LLVM lowering resolves to
  // (byte_offset, byte_size) via DataLayout.
  // nullopt = observe full slot (backward-compatible default).
  std::optional<PlaceId> observed_place;
  // Optional late-bound index for dynamic bit-select edge triggers.
  // When set, codegen emits dynamic byte_offset/bit_index computation and
  // runtime creates a rebind subscription on the index variable's slot.
  std::optional<LateBoundIndex> late_bound;
};

// Event wait suspension (requires scheduler).
struct Wait {
  std::vector<WaitTrigger> triggers;  // OR semantics
  BasicBlockId resume = {};           // Block to resume after trigger fires
};

// Return terminator - ends execution of a function or process.
// For functions: value must be present (functions always return a value).
// For processes: value is empty (processes don't return values).
struct Return {
  std::optional<Operand> value;
};

// Use shared TerminationKind enum from common header (ABI contract with
// runtime)
using ::lyra::common::TerminationKind;

// Simulation termination ($finish, $fatal, $stop, $exit).
struct Finish {
  TerminationKind kind;
  int level;  // 0 = silent, 1 = print time, 2 = print time+stats
  std::optional<Operand>
      message;  // String handle for $fatal, nullopt otherwise
};

// Repeat loop back to entry block (requires scheduler).
struct Repeat {};

// Terminator data variant.
using TerminatorData = std::variant<
    Jump, Branch, Switch, QualifiedDispatch, Delay, Wait, Return, Finish,
    Repeat>;

// A block terminator that determines control flow.
struct Terminator {
  TerminatorData data;
  common::OriginId origin = common::OriginId::Invalid();
};

}  // namespace lyra::mir
