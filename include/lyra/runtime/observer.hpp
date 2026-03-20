#pragma once

#include <cstdint>
#include <vector>

namespace lyra::runtime {

// Captured specialization-local addressing state for observer programs.
// This is the dynamic instance-side context needed by specialization-local
// lowering. DesignState* is not captured; it remains invocation/runtime state.
//
// For design-global observers, all fields are zero/null.
struct ObserverContext {
  void* this_ptr = nullptr;
  uint32_t instance_id = 0;
  uint32_t signal_id_offset = 0;
};

// Observer program entrypoints.
// All observer programs receive (DesignState*, Engine*, ObserverContext*).
// Monitor-check programs additionally receive prev_buf*.
using StrobeProgramFn =
    void (*)(void* design_state, void* engine, const ObserverContext* ctx);

using MonitorCheckProgramFn = void (*)(
    void* design_state, void* engine, const ObserverContext* ctx,
    void* prev_buf);

// Per-registration runtime state for $strobe.
// Deferred one-shot observer execution at postponed region.
struct StrobeRecord {
  StrobeProgramFn program = nullptr;
  void* design_state = nullptr;
  ObserverContext context;
};

// Per-registration runtime state for $monitor.
// Persistent observer with snapshot comparison across time.
struct MonitorRecord {
  bool enabled = true;
  MonitorCheckProgramFn program = nullptr;
  void* design_state = nullptr;
  ObserverContext context;
  std::vector<uint8_t> prev_values;
};

}  // namespace lyra::runtime
