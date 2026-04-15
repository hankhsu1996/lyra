#pragma once

#include <cstddef>
#include <cstdint>
#include <vector>

namespace lyra::runtime {

struct RuntimeInstance;

// Captured specialization-local addressing state for observer programs.
// This is the dynamic instance-side context needed by specialization-local
// lowering. DesignState* is not captured; it remains invocation/runtime state.
//
// For design-global observers, all fields are nullptr.
// Layout must match the LLVM struct type in GetObserverContextStructType():
//   { ptr this_ptr, ptr instance }
struct ObserverContext {
  void* this_ptr = nullptr;
  RuntimeInstance* instance = nullptr;
};

static_assert(sizeof(ObserverContext) == 16, "ObserverContext size mismatch");
static_assert(
    alignof(ObserverContext) == 8, "ObserverContext alignment mismatch");
static_assert(
    offsetof(ObserverContext, this_ptr) == 0,
    "ObserverContext this_ptr offset mismatch");
static_assert(
    offsetof(ObserverContext, instance) == 8,
    "ObserverContext instance offset mismatch");

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
