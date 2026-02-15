#pragma once

#include <array>
#include <cstddef>
#include <cstdint>

#include "lyra/runtime/index_plan.hpp"

namespace lyra::runtime {

// Suspend tag - matches SuspendReason variant index for consistency.
// Used in C ABI struct for both interpreter and LLVM backend communication.
enum class SuspendTag : uint8_t {
  kFinished = 0,
  kDelay = 1,
  kWait = 2,
  kRepeat = 3,
};

struct WaitTriggerRecord {
  uint32_t signal_id = 0;
  uint8_t edge = 0;       // common::EdgeKind
  uint8_t bit_index = 0;  // Bit position within observed byte (edge triggers)
  std::array<uint8_t, 2> padding = {};
  uint32_t byte_offset = 0;  // Observation start within slot
  uint32_t byte_size = 0;    // Observation size; 0 = full slot
};

static_assert(
    sizeof(WaitTriggerRecord) == 16, "WaitTriggerRecord size mismatch");
static_assert(
    alignof(WaitTriggerRecord) == 4, "WaitTriggerRecord alignment mismatch");
static_assert(
    offsetof(WaitTriggerRecord, bit_index) == 5,
    "WaitTriggerRecord bit_index offset mismatch");
static_assert(
    offsetof(WaitTriggerRecord, byte_offset) == 8,
    "WaitTriggerRecord byte_offset offset mismatch");
static_assert(
    offsetof(WaitTriggerRecord, byte_size) == 12,
    "WaitTriggerRecord byte_size offset mismatch");

// Late-bound header for dynamic-index edge triggers.
// References into plan_ops and dep_slots pools via start/count spans.
struct LateBoundHeader {
  uint32_t trigger_index = 0;
  uint16_t plan_ops_start = 0;
  uint16_t plan_ops_count = 0;
  uint16_t dep_slots_start = 0;
  uint16_t dep_slots_count = 0;
  int32_t index_base = 0;
  int32_t index_step = 1;
  uint32_t total_bits = 0;
};

static_assert(sizeof(LateBoundHeader) == 24, "LateBoundHeader size mismatch");
static_assert(
    alignof(LateBoundHeader) == 4, "LateBoundHeader alignment mismatch");
static_assert(
    offsetof(LateBoundHeader, plan_ops_start) == 4,
    "LateBoundHeader plan_ops_start offset mismatch");
static_assert(
    offsetof(LateBoundHeader, dep_slots_start) == 8,
    "LateBoundHeader dep_slots_start offset mismatch");
static_assert(
    offsetof(LateBoundHeader, index_base) == 12,
    "LateBoundHeader index_base offset mismatch");
static_assert(
    offsetof(LateBoundHeader, index_step) == 16,
    "LateBoundHeader index_step offset mismatch");
static_assert(
    offsetof(LateBoundHeader, total_bits) == 20,
    "LateBoundHeader total_bits offset mismatch");

// Performance knob: triggers <= this use inline storage (no heap).
// NOT a hard limit - larger lists use heap allocation.
static constexpr uint32_t kInlineTriggerCapacity = 32;

// C ABI layout for suspend record - used by both interpreter and LLVM.
// Process writes this before returning; runtime reads it after return.
//
// Key invariant: (triggers_ptr, num_triggers) is the single source of truth.
// All code iterates via triggers_ptr[0..num_triggers) - never access
// inline_triggers directly except during setup in LyraSuspendWait.
//
// Storage policy:
// - num_triggers <= kInlineTriggerCapacity: triggers_ptr = &inline_triggers[0]
// - num_triggers > kInlineTriggerCapacity: triggers_ptr = heap-allocated buffer
//
// Heap-owned detection: triggers_ptr != nullptr && triggers_ptr !=
// inline_triggers.data()
struct SuspendRecord {
  SuspendTag tag = SuspendTag::kFinished;
  uint64_t delay_ticks = 0;                   // For kDelay
  uint32_t resume_block = 0;                  // For kDelay, kWait, kRepeat
  uint32_t num_triggers = 0;                  // For kWait
  WaitTriggerRecord* triggers_ptr = nullptr;  // Single source of truth
  uint32_t num_late_bound = 0;                // For kWait (late-bound triggers)
  LateBoundHeader* late_bound_ptr = nullptr;
  uint32_t num_plan_ops = 0;
  IndexPlanOp* plan_ops_ptr = nullptr;
  uint32_t num_dep_slots = 0;
  uint32_t* dep_slots_ptr = nullptr;
  std::array<WaitTriggerRecord, kInlineTriggerCapacity> inline_triggers = {};
};

// Layout invariants (relative ordering and alignment, not absolute offsets)
static_assert(alignof(SuspendRecord) == 8, "SuspendRecord alignment mismatch");
static_assert(
    offsetof(SuspendRecord, triggers_ptr) <
        offsetof(SuspendRecord, inline_triggers),
    "triggers_ptr must precede inline_triggers");
static_assert(
    offsetof(SuspendRecord, inline_triggers) % alignof(WaitTriggerRecord) == 0,
    "inline_triggers must be aligned for WaitTriggerRecord");

}  // namespace lyra::runtime
