#pragma once

#include <cstdint>
#include <type_traits>
#include <vector>

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
struct ScheduledEvent {
  ProcessHandle handle;
  ResumePoint resume;
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

// Callback for Postponed region ($strobe, future $monitor, etc.).
// Called at end of time slot to re-evaluate and print with final values.
// Matches user function ABI: void (DesignState*, Engine*)
using PostponedCallback = void (*)(void*, void*);

// Monitor check callback: evaluates expressions, compares with prev buffer.
// void check_thunk(DesignState*, Engine*, prev_buffer*)
using MonitorCheckCallback = void (*)(void*, void*, void*);

// State for active $monitor (only one can be active at a time per IEEE 1800).
struct MonitorState {
  bool enabled = true;
  MonitorCheckCallback check_thunk = nullptr;
  void* design_state = nullptr;
  std::vector<uint8_t> prev_values;  // Runtime-owned prev buffer
};

// Postponed queue entry: callback + captured context.
struct PostponedRecord {
  PostponedCallback callback;
  void* design_state;  // DesignState*
};

}  // namespace lyra::runtime
