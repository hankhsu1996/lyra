#pragma once

#include <optional>
#include <variant>
#include <vector>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/origin_id.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/mir/assoc_op.hpp"
#include "lyra/mir/builtin.hpp"
#include "lyra/mir/call.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/rhs.hpp"

namespace lyra::mir {

// Unified write target: either a local addressable Place or a non-local
// external reference (recipe handle resolved at construction time).
// All assignment statements use this instead of separate statement kinds
// per destination carrier.
using WriteTarget = std::variant<PlaceId, ExternalRefId>;

// Write mode for direct assignment statements.
// Encodes lifecycle policy decided at MIR construction time.
enum class WriteMode : uint8_t {
  kPlain,  // No lifecycle (non-managed types only)
  kCopy,   // Destroy old + clone new (source preserved)
  kMove,   // Destroy old + move new (+ cleanup source if applicable)
};

// PlainAssign: immediate write with no lifecycle (dest := rhs).
// For non-managed trivial types only. Backend emits raw store.
struct PlainAssign {
  WriteTarget dest;
  RightHandSide rhs;
};

// CopyAssign: immediate write with clone semantics (dest := clone(rhs)).
// Destination is already-initialized managed storage.
// Backend: destroy old contents, clone/retain new value from source.
// Source remains valid after the operation.
struct CopyAssign {
  WriteTarget dest;
  RightHandSide rhs;
};

// MoveAssign: immediate write with move semantics (dest := move(rhs)).
// Destination is already-initialized managed storage.
// Backend: destroy old contents, move new value from source.
// Source is cleaned up (nulled) after the operation if applicable.
struct MoveAssign {
  WriteTarget dest;
  RightHandSide rhs;
};

// Type trait for direct-assign statements (PlainAssign, CopyAssign,
// MoveAssign).
template <typename T>
inline constexpr bool kIsDirectAssign =
    std::is_same_v<T, PlainAssign> || std::is_same_v<T, CopyAssign> ||
    std::is_same_v<T, MoveAssign>;

// Read-only view into a direct-assign statement's common fields.
// Returned by TryGetDirectAssign for uniform access without caring about mode.
struct DirectAssignRef {
  const WriteTarget* dest;
  const RightHandSide* rhs;

  explicit operator bool() const {
    return dest != nullptr;
  }
};

// Mutable view into a direct-assign statement's common fields.
struct MutableDirectAssignRef {
  WriteTarget* dest;
  RightHandSide* rhs;

  explicit operator bool() const {
    return dest != nullptr;
  }
};

// TryGetDirectAssign: declared after StatementData definition below.

// GuardedAssign: conditional write (if guard then dest := rhs).
// Semantics: rhs is always evaluated; only the write is gated by guard.
// For short-circuit semantics (rhs has side effects), use control flow.
struct GuardedAssign {
  WriteTarget dest;
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
  WriteTarget dest;
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
//    - LLVM: CommitRuntimeResult(dest, Load(tmp))
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

// DefineTemp: define an SSA temp value (no storage).
// The temp_id is bound to the computed rhs value.
// Use Operand::UseTemp(temp_id) to reference this temp.
// INVARIANT: temp_id must be allocated as TempKind::kValue.
struct DefineTemp {
  int temp_id;        // SSA temp id being defined
  TypeId type;        // Type of the value (must match temp_metadata[temp_id])
  RightHandSide rhs;  // Computation that produces the value
};

// DpiCall: foreign-boundary invocation for DPI-C imported functions.
//
// Separate call family from Call. DPI has its own boundary semantics:
// per-argument direction, ABI carrier passing mode, staged C-ABI temps for
// output/inout, and post-call writeback through the design-state commit path.
//
// Return handling reuses CallReturn (same staging discipline as Call).
struct DpiCall {
  DpiImportRef callee;
  std::vector<DpiArgBinding> args;
  std::optional<CallReturn> ret;
};

// Write-target helpers. Centralize all WriteTarget dispatch so that
// individual analysis/lowering files do not re-decide the abstraction split.

[[nodiscard]] inline auto GetLocalPlace(const WriteTarget& t)
    -> std::optional<PlaceId> {
  if (const auto* p = std::get_if<PlaceId>(&t)) {
    return *p;
  }
  return std::nullopt;
}

[[nodiscard]] inline auto IsExternalWrite(const WriteTarget& t) -> bool {
  return std::holds_alternative<ExternalRefId>(t);
}

template <class FPlace, class FExt>
auto VisitWriteTarget(const WriteTarget& t, FPlace&& on_place, FExt&& on_ext) {
  if (const auto* p = std::get_if<PlaceId>(&t)) {
    return std::forward<FPlace>(on_place)(*p);
  }
  return std::forward<FExt>(on_ext)(std::get<ExternalRefId>(t));
}

// Checked backend boundary helper. Asserts that a WriteTarget is a local
// PlaceId. Throws InternalError if an ExternalRefId reaches backend code
// that requires bound local targets. Use this instead of raw std::get.
[[nodiscard]] inline auto RequireLocalDest(
    const WriteTarget& t, const char* where) -> PlaceId {
  if (const auto* p = std::get_if<PlaceId>(&t)) {
    return *p;
  }
  throw common::InternalError(
      where, "backend received Assign with ExternalRefId destination");
}

// Named event trigger (wakes all processes waiting on the event).
struct TriggerEvent {
  EventId event;
};

// Initialize: first-write initialization of a place (dest := value).
//
// Unlike CopyAssign/MoveAssign, Initialize does NOT destroy/release previous
// contents. It assumes the destination storage is uninitialized (no live
// value). Used by MIR prologue synthesis for SV default initialization of
// function-local storage.
//
// The backend lowers this as a pure store: no lifecycle dispatch,
// no destroy-before-write, no copy-assign/move-assign semantics.
struct Initialize {
  PlaceId dest;
  Operand value;
};

// Statement data variant.
using StatementData = std::variant<
    PlainAssign, CopyAssign, MoveAssign, GuardedAssign, Effect, DeferredAssign,
    Call, DpiCall, BuiltinCall, DefineTemp, AssocOp, TriggerEvent, Initialize>;

// A statement that does not affect control flow.
// - PlainAssign, CopyAssign, MoveAssign, GuardedAssign, DeferredAssign write
// - Effect produces side effects but no value
struct Statement {
  StatementData data;
  common::OriginId origin = common::OriginId::Invalid();
};

// Generic operand walker. Visits every Operand reachable from a statement's
// value-producing positions: RHS operands, guard operands, call input
// arguments, and associative-array key/value operands. Does NOT visit
// write targets (use ForEachWriteTarget for those). Use this for
// centralized operand traversal rather than hand-maintained lists.
template <class F>
void ForEachOperand(const RightHandSide& rhs, const F& f) {
  std::visit(
      common::Overloaded{
          [&](const Operand& op) { f(op); },
          [&](const Rvalue& rv) {
            for (const auto& op : rv.operands) {
              f(op);
            }
          },
      },
      rhs);
}

template <class F>
void ForEachOperand(const StatementData& stmt, const F& f) {
  std::visit(
      common::Overloaded{
          [&](const auto& a)
            requires(kIsDirectAssign<std::decay_t<decltype(a)>>)
          { ForEachOperand(a.rhs, f); },
          [&](const GuardedAssign& a) {
            ForEachOperand(a.rhs, f);
            f(a.guard);
          },
          [&](const DeferredAssign& a) { ForEachOperand(a.rhs, f); },
          [&](const DefineTemp& dt) { ForEachOperand(dt.rhs, f); },
          [&](const Call& c) {
            for (const auto& op : c.in_args) {
              f(op);
            }
          },
          [&](const DpiCall& c) {
            for (const auto& arg : c.args) {
              if (arg.input_value) f(*arg.input_value);
            }
          },
          [&](const BuiltinCall& c) {
            for (const auto& op : c.args) {
              f(op);
            }
          },
          [&](const AssocOp& a) {
            std::visit(
                common::Overloaded{
                    [&](const AssocGet& g) { f(g.key); },
                    [&](const AssocSet& s) {
                      f(s.key);
                      f(s.value);
                    },
                    [&](const AssocExists& e) { f(e.key); },
                    [](const AssocDelete&) {},
                    [&](const AssocDeleteKey& d) { f(d.key); },
                    [](const AssocNum&) {},
                    [](const AssocIterFirst&) {},
                    [](const AssocIterLast&) {},
                    [](const AssocIterNext&) {},
                    [](const AssocIterPrev&) {},
                    [](const AssocSnapshot&) {},
                },
                a.data);
          },
          [](const Effect&) {},
          [](const TriggerEvent&) {},
          [&](const Initialize& init) { f(init.value); },
          },
          stmt);
}

// Generic write-target walker. Visits every WriteTarget in a statement.
template <class F>
void ForEachWriteTarget(const StatementData& stmt, const F& f) {
  std::visit(
      common::Overloaded{
          [&](const auto& a)
            requires(kIsDirectAssign<std::decay_t<decltype(a)>>)
          { f(a.dest); },
          [&](const GuardedAssign& a) { f(a.dest); },
          [&](const DeferredAssign& a) { f(a.dest); },
          [&](const Initialize& init) { f(WriteTarget{init.dest}); },
          [](const auto&) {},
          },
          stmt);
}

// TryGetDirectAssign implementations (declared above, after forward decl).
inline auto TryGetDirectAssign(const StatementData& data) -> DirectAssignRef {
  if (const auto* a = std::get_if<PlainAssign>(&data))
    return {.dest = &a->dest, .rhs = &a->rhs};
  if (const auto* a = std::get_if<CopyAssign>(&data))
    return {.dest = &a->dest, .rhs = &a->rhs};
  if (const auto* a = std::get_if<MoveAssign>(&data))
    return {.dest = &a->dest, .rhs = &a->rhs};
  return {.dest = nullptr, .rhs = nullptr};
}

inline auto TryGetDirectAssign(StatementData& data) -> MutableDirectAssignRef {
  if (auto* a = std::get_if<PlainAssign>(&data))
    return {.dest = &a->dest, .rhs = &a->rhs};
  if (auto* a = std::get_if<CopyAssign>(&data))
    return {.dest = &a->dest, .rhs = &a->rhs};
  if (auto* a = std::get_if<MoveAssign>(&data))
    return {.dest = &a->dest, .rhs = &a->rhs};
  return {.dest = nullptr, .rhs = nullptr};
}

}  // namespace lyra::mir
