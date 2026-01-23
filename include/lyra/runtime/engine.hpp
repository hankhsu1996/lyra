#pragma once

#include <cstdint>
#include <functional>
#include <map>
#include <queue>
#include <vector>

#include "lyra/common/edge_kind.hpp"

namespace lyra::runtime {

// Simulation time in ticks (timescale-independent).
using SimTime = uint64_t;

// Unique identifier for a process instance.
// Combines process definition ID with instance path for hierarchical designs.
struct ProcessHandle {
  uint32_t process_id = 0;
  uint32_t instance_id = 0;  // For future hierarchy support

  auto operator==(const ProcessHandle&) const -> bool = default;
};

// Resume point within a process (block index + instruction index).
// Allows processes to suspend and resume at arbitrary points.
struct ResumePoint {
  uint32_t block_index = 0;
  uint32_t instruction_index = 0;
};

// Signal identifier for trigger subscription.
// Maps to design storage slot ID.
using SignalId = uint32_t;

// IEEE 1800 simulation regions (simplified).
// Active → Inactive → NBA is the core loop for RTL simulation.
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

// Forward declaration for callback
class Engine;

// Callback type for process execution.
// Engine calls this when a process should run; callback handles suspension.
// The callback should call Delay/Subscribe/etc. on the engine to reschedule.
using ProcessRunner = std::function<void(
    Engine& engine, ProcessHandle handle, ResumePoint resume)>;

// Simulation Engine: event-driven scheduler for SystemVerilog processes.
//
// Design:
// - Backend-agnostic: both MIR interpreter and LLVM-generated code can use it
// - IEEE 1800 stratified event scheduler (Active → Inactive → NBA regions)
// - Processes suspend via Delay/Subscribe, engine resumes them later
//
// Usage:
// 1. Create engine with a ProcessRunner callback
// 2. Schedule initial processes with ScheduleInitial()
// 3. Call Run() to execute until completion or time limit
class Engine {
 public:
  explicit Engine(ProcessRunner runner) : runner_(std::move(runner)) {
  }

  // Schedule a process to start at time 0 (for initial blocks).
  void ScheduleInitial(ProcessHandle handle);

  // Schedule a process to resume after a delay.
  // Called by interpreter/codegen when process hits a Delay terminator.
  void Delay(ProcessHandle handle, ResumePoint resume, SimTime ticks);

  // Schedule to inactive region (same time slot, #0 delay).
  void DelayZero(ProcessHandle handle, ResumePoint resume);

  // Subscribe to signal edge. Process resumes when signal changes.
  // Called by interpreter/codegen when process hits a Wait terminator.
  void Subscribe(
      ProcessHandle handle, ResumePoint resume, SignalId signal, EdgeKind edge);

  // Schedule process to resume in the next delta cycle (same time).
  // Used for kRepeat terminator.
  void ScheduleNextDelta(ProcessHandle handle, ResumePoint resume);

  // Notify that a signal changed (called after stores).
  // Wakes up subscribed processes into the next delta cycle.
  void NotifyChange(
      SignalId signal, bool old_lsb, bool new_lsb, bool value_changed);

  // Run simulation until completion or time limit.
  // Returns final simulation time.
  auto Run(SimTime max_time = 1'000'000) -> SimTime;

  // Get current simulation time.
  [[nodiscard]] auto CurrentTime() const -> SimTime {
    return current_time_;
  }

 private:
  void ExecuteTimeSlot();
  void ExecuteRegion(Region region);

  ProcessRunner runner_;
  SimTime current_time_ = 0;

  // Time-based scheduling: time → events
  std::map<SimTime, std::vector<ScheduledEvent>> delay_queue_;

  // Region queues for current time slot
  std::vector<ScheduledEvent> active_queue_;
  std::queue<ScheduledEvent> inactive_queue_;

  // Next-delta queue: events scheduled for the next delta cycle
  std::vector<ScheduledEvent> pending_queue_;

  // Trigger subscriptions: signal → waiting processes
  struct Waiter {
    ProcessHandle handle;
    ResumePoint resume;
    EdgeKind edge = EdgeKind::kAnyChange;
  };
  std::map<SignalId, std::vector<Waiter>> waiters_;
};

}  // namespace lyra::runtime
