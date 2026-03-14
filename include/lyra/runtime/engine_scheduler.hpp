#pragma once

#include <cstdint>
#include <type_traits>

#include "lyra/runtime/activation_trace.hpp"
#include "lyra/runtime/engine_types.hpp"
#include "lyra/runtime/small_byte_buffer.hpp"

namespace lyra::runtime {

// IEEE 1800 simulation regions (simplified).
// Active -> Inactive -> NBA is the core loop for RTL simulation.
enum class Region : uint8_t {
  kActive,    // Blocking assignments, $display
  kInactive,  // #0 delays (same time slot)
  kNBA,       // Nonblocking assignment updates
};

// Scheduled event: a process ready to resume at a specific point.
// Wake metadata is part of activation identity -- it describes why this
// activation exists, not debug/trace adornment.
struct ScheduledEvent {
  ProcessHandle handle;
  ResumePoint resume;
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
