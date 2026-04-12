#pragma once

#include <array>
#include <cstddef>
#include <cstdint>

#include "lyra/runtime/trigger_record.hpp"
#include "lyra/runtime/wait_site.hpp"

namespace lyra::runtime {

struct IndexPlanOp;

// Suspend tag - matches SuspendReason variant index for consistency.
// Used in C ABI struct for both interpreter and LLVM backend communication.
enum class SuspendTag : uint8_t {
  kFinished = 0,
  kDelay = 1,
  kWait = 2,
  kRepeat = 3,
  kWaitEvent = 4,
};

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
  // R5: Typed destination identity. When dst_is_local != 0,
  // dst_instance_id and dst_local_id carry the owning instance and
  // body-local signal identity. Runtime decodes into ConnectionTarget.
  uint8_t dst_is_local = 0;
  uint8_t padding = 0;
  uint32_t trigger_byte_offset = 0;  // WaitTriggerRecord.byte_offset
  uint32_t trigger_byte_size = 0;    // WaitTriggerRecord.byte_size
  uint32_t dst_instance_id = 0;  // InstanceId.value (valid when dst_is_local)
  uint32_t dst_local_id = 0;  // LocalSignalId.value (valid when dst_is_local)
  // R5: Typed trigger identity. Same pattern as destination.
  uint8_t trigger_is_local = 0;
  uint8_t padding2 = 0;
  uint16_t padding3 = 0;
  uint32_t trigger_instance_id = 0;
  uint32_t trigger_local_id = 0;
};

static_assert(
    sizeof(ConnectionDescriptor) == 48, "ConnectionDescriptor size mismatch");
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
  DepSignalRecord* dep_slots_ptr = nullptr;
  std::array<WaitTriggerRecord, kInlineTriggerCapacity> inline_triggers = {};
  uint32_t event_id = 0;  // For kWaitEvent
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
