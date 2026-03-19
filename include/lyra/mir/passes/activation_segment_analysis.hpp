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

enum class BoundaryKind : uint8_t {
  kObservation,
  kMayWriteAny,
  kWritebackSpecific,
};

struct BoundaryRecord {
  uint32_t block_index;
  uint32_t statement_index;
  BoundaryKind kind;
  std::vector<SignalRef> affected_slots;
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
  std::vector<BoundaryRecord> boundaries;
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
