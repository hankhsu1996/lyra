#pragma once

#include <cstdint>
#include <optional>
#include <variant>
#include <vector>

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
  PlaceId condition;
  BasicBlockId then_target;
  std::vector<Operand> then_args;  // Values for then_target.params
  BasicBlockId else_target;
  std::vector<Operand> else_args;  // Values for else_target.params
};

// Multi-way switch based on selector value.
struct Switch {
  PlaceId selector;
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
  std::vector<PlaceId> conditions;    // Pre-evaluated condition results
  std::vector<BasicBlockId> targets;  // One per condition + else (last)
  bool has_else;                      // Suppresses no-match warning for kUnique
};

// Time delay suspension (requires scheduler).
struct Delay {
  uint64_t ticks = 0;        // Canonical delay ticks (from HIR)
  BasicBlockId resume = {};  // Block to resume after delay
};

// A single trigger in a Wait terminator.
struct WaitTrigger {
  SlotId signal;
  common::EdgeKind edge = common::EdgeKind::kAnyChange;
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
