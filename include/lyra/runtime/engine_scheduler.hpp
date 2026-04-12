#pragma once

#include <cstdint>
#include <type_traits>
#include <variant>

#include "lyra/runtime/activation_trace.hpp"
#include "lyra/runtime/signal_coord.hpp"
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
  InstanceId instance_id;
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

// Typed NBA payloads. Each mode has its own struct with exactly the fields
// it needs, so field semantics never depend on a mode tag.

// Full overwrite: direct compare/copy of byte_size bytes at write_ptr.
struct NbaFullOverwrite {
  uint32_t byte_size;
  SmallByteBuffer value;
};

// Masked merge: per-byte (old & ~mask) | (new & mask).
struct NbaMaskedMerge {
  uint32_t byte_size;
  SmallByteBuffer value;
  SmallByteBuffer mask;
};

// Canonical packed two-plane narrow overwrite for 4-state byte-addressable
// packed subview writes. Writes region_byte_size bytes at write_ptr (value
// plane) and at write_ptr + second_region_offset (unknown plane).
struct NbaCanonicalPackedTwoPlane {
  uint32_t region_byte_size;
  uint32_t second_region_offset;
  SmallByteBuffer value;
  SmallByteBuffer unk;
};

using NbaPayload =
    std::variant<NbaFullOverwrite, NbaMaskedMerge, NbaCanonicalPackedTwoPlane>;

// R5: NBA notification signal identity. Typed at enqueue time, not
// reverse-engineered at commit time.
struct NbaNotifyGlobal {
  GlobalSignalId signal;
};
struct NbaNotifyLocal {
  uint32_t inst_idx = UINT32_MAX;
  LocalSignalId signal{};
};
using NbaNotifySignal = std::variant<NbaNotifyGlobal, NbaNotifyLocal>;

// Generic NBA queue entry: deferred write committed in the NBA region.
// Common header (write target + notification) plus typed payload.
//
// ARCHITECTURAL INVARIANT:
// The generic nba_queue_ serves only non-instance-owned targets:
//   - Global/package signals (NbaNotifyGlobal)
//   - Cross-instance local signals (NbaNotifyLocal where the writing
//     process belongs to a different instance than the target)
// Instance-owned local signals use per-instance deferred storage
// (RuntimeInstanceStorage::deferred_inline_base / deferred_appendix_base)
// committed by CommitDeferredLocalNbas, not this queue.
struct NbaEntry {
  void* write_ptr;
  const void* notify_base_ptr;
  NbaNotifySignal notify_signal;
  NbaPayload payload;
};

static_assert(
    std::is_nothrow_move_constructible_v<NbaEntry>,
    "NbaEntry must be nothrow-movable for efficient std::vector growth");

// Forward declaration for callback
class Engine;

}  // namespace lyra::runtime
