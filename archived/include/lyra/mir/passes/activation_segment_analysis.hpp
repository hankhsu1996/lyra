#pragma once

#include <cstdint>
#include <optional>
#include <vector>

#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/routine.hpp"
#include "lyra/mir/terminator.hpp"

namespace lyra::mir::passes {

enum class AccessShape : uint8_t {
  kWholeSlotRead,
  kWholeSlotWrite,
  kProjectedRead,
  kProjectedWrite,
  kDeferredWrite,
  // Slot address passed to external code with unknown access pattern.
  // Example: inout parameter passing a module-slot pointer to a function.
  kAddressEscape,
  // Slot is the explicit target of an external write operation.
  // The write destination is known and enumerable.
  // Example: $readmemh target, $fscanf writeback destination.
  kExternalWriteTarget,
};

struct SlotAccessRecord {
  SignalRef slot;
  AccessShape shape;
  TypeId root_type;
  uint32_t block_index;
  uint32_t statement_index;
};

// How broadly a canonical write can invalidate managed shadow coherence.
// Only meaningful when writes_canonical_state is true.
enum class CanonicalWriteFootprint : uint8_t {
  kNone,
  kSpecific,
  kAll,
};

// Factual description of a statement/effect's canonical-state interaction
// that is relevant to activation-local managed coherence. Produced by
// semantic analysis. This is NOT a policy/action object.
//
// The fact layer answers: "does this statement interact with canonical
// slot storage through a path that can affect managed-shadow coherence?"
// It does not answer: "what sync/reload should the executor perform?"
//
// Note: ordinary module-slot Assign/GuardedAssign do write canonical
// storage (for non-managed targets), but that write does not affect
// managed-shadow coherence -- the managed slot's shadow remains the
// authoritative copy and dirty-marks are processed after the body
// returns. These statements produce no fact entry.
//
// Statements with no managed-coherence-relevant interaction produce no
// fact entry. The absence of an entry means the statement has no
// activation-local contract relevance. Do not add entries for
// statements that have no managed-coherence impact -- that would
// rebuild the old taxonomy drift.
//
// Invariants:
//   writes_canonical_state == false implies write_footprint == kNone
//   writes_canonical_state == true implies write_footprint != kNone
//   write_targets non-empty only when write_footprint == kSpecific
struct StatementSemanticFact {
  uint32_t block_index;
  uint32_t statement_index;

  // The statement's lowering/runtime path reads canonical slot storage
  // through a side path (not through the resolver), requiring managed
  // values to be published to canonical first. Callees that access
  // canonical state via design_ptr set this to true.
  bool reads_canonical_state;

  // The statement's lowering/runtime path writes canonical slot storage
  // through an external path (runtime call, direct pointer) that
  // bypasses the resolver, which can make managed shadow state stale.
  bool writes_canonical_state;

  // Scope of canonical write impact. Must be kSpecific or kAll when
  // writes_canonical_state is true.
  CanonicalWriteFootprint write_footprint;

  // Specific canonical targets affected when write_footprint == kSpecific.
  std::vector<SignalRef> write_targets;

  // Analysis could not prove a narrower fact. The contract planner must
  // use conservative policy (sync before + reload all after).
  bool conservative_unknown;
};

// Raw observer-registration record.
// Kept as a raw fact on the segment; segment blockers are derived from
// these in EvaluateSegmentEligibility.
struct ObserverRegistration {
  uint32_t block_index;
  uint32_t statement_index;
};

struct ActivationSegment {
  uint32_t entry_block;
  std::vector<uint32_t> blocks;
  std::vector<StatementSemanticFact> semantic_facts;
  std::vector<SlotAccessRecord> slot_accesses;
  std::vector<ObserverRegistration> observer_registrations;
};

enum class SegmentBlocker : uint8_t {
  kPersistentObserverRegistration,
};

struct SegmentEligibility {
  bool eligible = false;
  std::optional<SegmentBlocker> reason;
};

enum class SlotIneligibilityReason : uint8_t {
  kDesignGlobal,
  kUnsupportedType,
  kEnumNonIntegralBase,
  // Slot has projected access (read or write through field/index/bit-range).
  // v1 only allows whole-slot reads and writes.
  kProjectedAccess,
  kDeferredWrite,
  kAddressEscape,
  kExternalWriteTarget,
  kReadOnly,
};

struct SlotEligibility {
  SignalRef slot = {};
  TypeId root_type;
  bool eligible = false;
  std::optional<SlotIneligibilityReason> reason;
};

auto AnalyzeActivationSegments(const Process& process, const Arena& arena)
    -> std::vector<ActivationSegment>;

auto EvaluateSegmentEligibility(const ActivationSegment& segment)
    -> SegmentEligibility;

auto EvaluateSlotEligibility(
    const ActivationSegment& segment, const TypeArena& types)
    -> std::vector<SlotEligibility>;

}  // namespace lyra::mir::passes
