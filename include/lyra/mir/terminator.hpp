#pragma once

#include <cstdint>
#include <optional>
#include <span>
#include <variant>
#include <vector>

#include "lyra/common/bit_target_mapping.hpp"
#include "lyra/common/edge_kind.hpp"
#include "lyra/common/index_plan.hpp"
#include "lyra/common/origin_id.hpp"
#include "lyra/common/overloaded.hpp"
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

// Scoped slot reference for index plan dependencies.
// Preserves module-local vs design-global distinction end-to-end.
// Resolved to concrete design-global IDs at codegen time.
struct ScopedSlotRef {
  enum class Scope : uint8_t { kModuleLocal, kDesignGlobal };
  Scope scope;
  uint32_t id;

  auto operator==(const ScopedSlotRef&) const -> bool = default;

  auto operator<=>(const ScopedSlotRef& other) const {
    if (auto cmp =
            static_cast<uint8_t>(scope) <=> static_cast<uint8_t>(other.scope);
        cmp != 0) {
      return cmp;
    }
    return id <=> other.id;
  }
};

// MIR-level index plan operation that preserves storage scope.
// Wraps runtime::IndexPlanOp with scope info for kReadSlot ops.
// At codegen time, module-local slot IDs are resolved to design-global
// through the instance placement context.
struct ScopedPlanOp {
  runtime::IndexPlanOp op;
  ScopedSlotRef::Scope slot_scope = ScopedSlotRef::Scope::kDesignGlobal;
};

// Late-bound index metadata for dynamic-index edge triggers.
// Carries the expression plan (bytecode) to evaluate the index at runtime,
// the set of design-state slots the expression depends on (for rebind
// subscriptions), and the affine mapping from SV index to storage bit.
struct LateBoundIndex {
  std::vector<ScopedPlanOp> plan;        // Expression bytecode with scope
  std::vector<ScopedSlotRef> dep_slots;  // Scoped slots to subscribe
  runtime::BitTargetMapping mapping;
  // Element type for unpacked array or container edge triggers.
  // For unpacked arrays: mapping.index_step is a direction sign (+1/-1),
  // codegen multiplies by element_bit_stride from DataLayout.
  // For containers: codegen computes elem_stride from DataLayout.
  std::optional<TypeId> element_type;
  uint32_t num_elements = 0;  // 0 for containers (dynamic size)
  bool is_container = false;
};

// Scoped signal reference that distinguishes module-local vs design-global.
// Allows a single process to legally contain mixed-scope sensitivity while
// keeping MIR clean and future-shareable.
struct SignalRef {
  enum class Scope : uint8_t { kModuleLocal, kDesignGlobal };
  Scope scope;
  uint32_t
      id;  // body-local index (kModuleLocal) or global slot (kDesignGlobal)

  auto operator==(const SignalRef&) const -> bool = default;
};

// A single trigger in a Wait terminator.
struct WaitTrigger {
  SignalRef signal;
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

// Restart from process entry block (requires scheduler).
//
// Semantics: unconditionally transfer control back to the process entry block.
// Unlike Jump, this does not name a target -- the target is always the process
// entry. This is the only back-edge mechanism for looping processes.
//
// The runtime implements this by re-entering the process dispatch loop, which
// clears any installed wait state before re-executing the entry block.
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

// A control-flow edge: target block + values passed to its block params.
struct TerminatorSuccessor {
  BasicBlockId target;
  std::span<const Operand> args;
};

// Iterate every control-flow successor edge with its edge args.
template <typename F>
void ForEachSuccessor(const Terminator& term, const F& func) {
  std::visit(
      common::Overloaded{
          [&](const Jump& j) {
            func(TerminatorSuccessor{.target = j.target, .args = j.args});
          },
          [&](const Branch& b) {
            func(
                TerminatorSuccessor{
                    .target = b.then_target, .args = b.then_args});
            func(
                TerminatorSuccessor{
                    .target = b.else_target, .args = b.else_args});
          },
          [&](const Switch& s) {
            for (auto t : s.targets) {
              func(TerminatorSuccessor{.target = t, .args = {}});
            }
          },
          [&](const QualifiedDispatch& qd) {
            for (auto t : qd.targets) {
              func(TerminatorSuccessor{.target = t, .args = {}});
            }
          },
          [&](const Delay& d) {
            func(TerminatorSuccessor{.target = d.resume, .args = {}});
          },
          [&](const Wait& w) {
            func(TerminatorSuccessor{.target = w.resume, .args = {}});
          },
          [](const Return&) {},
          [](const Finish&) {},
          [](const Repeat&) {},
      },
      term.data);
}

// Iterate every operand local to the terminator (not an edge arg).
template <typename F>
void ForEachLocalOperand(const Terminator& term, const F& func) {
  std::visit(
      common::Overloaded{
          [](const Jump&) {},
          [&](const Branch& b) { func(b.condition); },
          [&](const Switch& s) { func(s.selector); },
          [&](const QualifiedDispatch& qd) {
            for (const auto& c : qd.conditions) {
              func(c);
            }
          },
          [](const Delay&) {},
          [](const Wait&) {},
          [&](const Return& r) {
            if (r.value) func(*r.value);
          },
          [&](const Finish& f) {
            if (f.message) func(*f.message);
          },
          [](const Repeat&) {},
      },
      term.data);
}

}  // namespace lyra::mir
