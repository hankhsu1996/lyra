#pragma once

#include <array>
#include <cstddef>
#include <cstdint>

#include "lyra/runtime/index_plan.hpp"
#include "lyra/runtime/wait_site.hpp"

namespace lyra::runtime {

// Suspend tag - matches SuspendReason variant index for consistency.
// Used in C ABI struct for both interpreter and LLVM backend communication.
enum class SuspendTag : uint8_t {
  kFinished = 0,
  kDelay = 1,
  kWait = 2,
  kRepeat = 3,
};

// Trigger install kind: explicit per-trigger classification written by codegen.
// Runtime installs subscriptions directly from this kind without inferring it
// from EdgeKind or late-bound header presence.
enum class TriggerInstallKind : uint8_t {
  kEdge = 0,
  kChange = 1,
  kContainer = 2,
};

// Trigger flags: explicit per-trigger metadata written by codegen.
inline constexpr uint8_t kTriggerInitiallyActive = 0x01;

struct WaitTriggerRecord {
  uint32_t signal_id = 0;
  uint8_t edge = 0;  // common::EdgeKind
  // kEdge/kChange: bit position within observed byte.
  // kContainer: must be 0 (unused).
  uint8_t bit_index = 0;
  uint8_t kind = 0;   // TriggerInstallKind
  uint8_t flags = 0;  // kTriggerInitiallyActive

  // Per-kind semantics:
  //   kEdge/kChange: byte offset within slot (observation start).
  //   kContainer: element index (sv_index). Runtime uses this directly
  //     as the logical container element index, not as a byte offset.
  uint32_t byte_offset = 0;

  // Per-kind semantics:
  //   kEdge/kChange: observation size in bytes; 0 = full slot.
  //   kContainer: unused (set to 0).
  uint32_t byte_size = 0;

  // For kContainer only. Element stride in bytes. 0 for non-container triggers.
  uint32_t container_elem_stride = 0;
};

static_assert(
    sizeof(WaitTriggerRecord) == 20, "WaitTriggerRecord size mismatch");
static_assert(
    alignof(WaitTriggerRecord) == 4, "WaitTriggerRecord alignment mismatch");
static_assert(
    offsetof(WaitTriggerRecord, bit_index) == 5,
    "WaitTriggerRecord bit_index offset mismatch");
static_assert(
    offsetof(WaitTriggerRecord, kind) == 6,
    "WaitTriggerRecord kind offset mismatch");
static_assert(
    offsetof(WaitTriggerRecord, flags) == 7,
    "WaitTriggerRecord flags offset mismatch");
static_assert(
    offsetof(WaitTriggerRecord, byte_offset) == 8,
    "WaitTriggerRecord byte_offset offset mismatch");
static_assert(
    offsetof(WaitTriggerRecord, byte_size) == 12,
    "WaitTriggerRecord byte_size offset mismatch");
static_assert(
    offsetof(WaitTriggerRecord, container_elem_stride) == 16,
    "WaitTriggerRecord container_elem_stride offset mismatch");

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
  uint32_t container_elem_stride = 0;  // 0 = not container, >0 = container mode
};

static_assert(sizeof(LateBoundHeader) == 28, "LateBoundHeader size mismatch");
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
static_assert(
    offsetof(LateBoundHeader, container_elem_stride) == 24,
    "LateBoundHeader container_elem_stride offset mismatch");

// Descriptor for kernelized connection processes.
// Stored in the process frame; the single runtime kernel reads these fields
// instead of emitting per-process LLVM IR.
struct ConnectionDescriptor {
  uint32_t src_slot_id = 0;       // Source slot ID for address resolution
  uint32_t dst_slot_id = 0;       // Dest slot ID for address resolution
  uint32_t byte_size = 0;         // Copy size
  uint32_t trigger_slot_id = 0;   // WaitTriggerRecord.signal_id
  uint8_t trigger_edge = 0;       // WaitTriggerRecord.edge
  uint8_t trigger_bit_index = 0;  // WaitTriggerRecord.bit_index
  uint16_t padding = 0;
  uint32_t trigger_byte_offset = 0;  // WaitTriggerRecord.byte_offset
  uint32_t trigger_byte_size = 0;    // WaitTriggerRecord.byte_size
};

static_assert(
    sizeof(ConnectionDescriptor) == 28, "ConnectionDescriptor size mismatch");
static_assert(
    alignof(ConnectionDescriptor) == 4,
    "ConnectionDescriptor alignment mismatch");

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
  uint64_t delay_ticks = 0;                      // For kDelay
  uint32_t resume_block = 0;                     // For kDelay, kWait, kRepeat
  WaitSiteId wait_site_id = kInvalidWaitSiteId;  // For kWait: compiled site
  uint32_t num_triggers = 0;                     // For kWait
  WaitTriggerRecord* triggers_ptr = nullptr;     // Single source of truth
  uint32_t num_late_bound = 0;  // For kWait (late-bound triggers)
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
