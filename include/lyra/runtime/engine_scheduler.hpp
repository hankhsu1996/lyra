#pragma once

#include <cstdint>
#include <vector>

#include "lyra/runtime/engine_types.hpp"

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

// NBA queue entry: deferred write with byte-level masking.
struct NbaEntry {
  void* write_ptr;              // Exact write address
  const void* notify_base_ptr;  // Slot root pointer (for offset computation)
  uint32_t byte_size;           // Size of write region at write_ptr
  uint32_t notify_slot_id;      // Slot ID for trigger lookup
  std::vector<uint8_t> value;   // New value bytes (storage layout)
  std::vector<uint8_t> mask;    // Byte mask (0 = preserve, 0xFF = overwrite)
};

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
