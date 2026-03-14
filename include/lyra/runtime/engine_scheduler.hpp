#pragma once

#include <cstdint>
#include <type_traits>

#include "lyra/runtime/activation_trace.hpp"
#include "lyra/runtime/small_byte_buffer.hpp"

namespace lyra::runtime {

// IEEE 1800 simulation regions (simplified).
// Active -> Inactive -> NBA is the core loop for RTL simulation.
enum class Region : uint8_t {
  kActive,    // Blocking assignments, $display
  kInactive,  // #0 delays (same time slot)
  kNBA,       // Nonblocking assignment updates
};

// Hot queue entry for process wakeup (12 bytes). Contains only the fields
// needed for dispatch. Trace-only fields (cause, trigger_slot) are stored
// per-process in wake_trace_ when activation tracing is enabled.
struct WakeupEntry {
  uint32_t process_id;
  uint32_t instance_id;
  uint32_t resume_block;
};

// Trace-only wakeup annotation. Stored per-process (indexed by process_id),
// populated at enqueue time only when activation tracing is enabled.
//
// Per-process storage is safe because each process has at most one pending
// wake source at a time:
//   - Delay/DelayZero/ScheduleNextDelta: called after ResetInstalledWait
//     clears all subscriptions, so flush cannot race with EnqueueProcessWakeup.
//   - EnqueueProcessWakeup: is_enqueued dedup prevents multiple writes
//     per delta.
//   - ScheduleInitial: one-time at startup, before any flush activity.
struct WakeTraceInfo {
  WakeCause cause = WakeCause::kDelay;
  uint32_t trigger_slot = kNoTriggerSlot;
};

// NBA write mode: distinguishes full-width overwrite from partial masked merge.
// Explicit in the type so runtime logic branches on meaning, not storage state.
enum class NbaWriteMode : uint8_t {
  kFullOverwrite,  // Direct compare/copy (most common)
  kMaskedMerge,    // Per-byte mask: (old & ~mask) | (new & mask)
};

// NBA queue entry: deferred write committed in the NBA region.
//
// Invariants:
//   value.Size() == byte_size (always)
//   kFullOverwrite: mask is empty (mask.Size() == 0)
//   kMaskedMerge:   mask.Size() == byte_size
struct NbaEntry {
  void* write_ptr;              // Exact write address
  const void* notify_base_ptr;  // Slot root pointer (for offset computation)
  uint32_t byte_size;           // Size of write region at write_ptr
  uint32_t notify_slot_id;      // Slot ID for trigger lookup
  NbaWriteMode mode = NbaWriteMode::kFullOverwrite;
  SmallByteBuffer value;  // New value bytes; size == byte_size
  SmallByteBuffer mask;   // Byte mask; populated iff mode == kMaskedMerge
};

static_assert(
    std::is_nothrow_move_constructible_v<NbaEntry>,
    "NbaEntry must be nothrow-movable for efficient std::vector growth");

// Forward declaration for callback
class Engine;

}  // namespace lyra::runtime
