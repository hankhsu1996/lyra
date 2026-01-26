#pragma once

#include <cstdint>
#include <functional>
#include <map>
#include <optional>
#include <queue>
#include <span>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>
#include <vector>

#include "lyra/common/edge_kind.hpp"
#include "lyra/common/time_format.hpp"
#include "lyra/runtime/file_manager.hpp"

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

// Waitable signal identifier. This IS the design storage slot ID —
// NotifyChange, Subscribe, and NBA commit all use the same ID space.
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

// NBA queue entry: deferred write with byte-level masking.
struct NbaEntry {
  void* write_ptr;             // Exact write address
  uint32_t byte_size;          // Size of write region at write_ptr
  uint32_t notify_slot_id;     // Slot ID for trigger lookup
  std::vector<uint8_t> value;  // New value bytes (storage layout)
  std::vector<uint8_t> mask;   // Byte mask (0 = preserve, 0xFF = overwrite)
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

  Engine(ProcessRunner runner, std::span<const std::string> plusargs)
      : runner_(std::move(runner)),
        plusargs_(plusargs.begin(), plusargs.end()) {
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
      ProcessHandle handle, ResumePoint resume, SignalId signal,
      common::EdgeKind edge);

  // Schedule process to resume in the next delta cycle (same time).
  // Used for kRepeat terminator.
  void ScheduleNextDelta(ProcessHandle handle, ResumePoint resume);

  // Enqueue a non-blocking assignment for later commit in the NBA region.
  void ScheduleNba(
      void* write_ptr, const void* notify_base_ptr, const void* value_ptr,
      const void* mask_ptr, uint32_t byte_size, uint32_t notify_slot_id);

  // Schedule a callback for the Postponed region ($strobe, future $monitor).
  // Callback executes at end of time slot with final signal values.
  void SchedulePostponed(PostponedCallback callback, void* design_state);

  // Notify that a signal changed (called after stores).
  // Wakes up subscribed processes into the next delta cycle.
  void NotifyChange(
      SignalId signal, bool old_lsb, bool new_lsb, bool value_changed);

  // Run simulation until completion or time limit.
  // Returns final simulation time.
  auto Run(SimTime max_time = 1'000'000) -> SimTime;

  // Stop the simulation ($finish semantics).
  void Finish() {
    finished_ = true;
  }

  // Get current simulation time.
  [[nodiscard]] auto CurrentTime() const -> SimTime {
    return current_time_;
  }

  // Set global precision power (called once at simulation init).
  void SetGlobalPrecision(int8_t power) {
    global_precision_power_ = power;
  }

  // Get global precision power.
  [[nodiscard]] auto GetGlobalPrecision() const -> int8_t {
    return global_precision_power_;
  }

  // Set time format state (called by $timeformat).
  void SetTimeFormat(
      int8_t units, int precision, std::string suffix, int min_width) {
    time_format_.units = units;
    time_format_.precision = precision;
    time_format_.suffix = std::move(suffix);
    time_format_.min_width = min_width;
  }

  // Get time format state.
  [[nodiscard]] auto GetTimeFormat() const -> const common::TimeFormatState& {
    return time_format_;
  }

  // Get file manager for $fopen/$fclose operations.
  [[nodiscard]] auto GetFileManager() -> FileManager& {
    return file_manager_;
  }

  // Plusargs query interface for $test$plusargs and $value$plusargs.
  // Returns 1 if prefix matches any plusarg, 0 otherwise.
  [[nodiscard]] auto TestPlusargs(std::string_view query) const -> int32_t;

  // $value$plusargs with integer output (%d format).
  // Returns 1 if match found, 0 otherwise. Writes parsed value to output.
  [[nodiscard]] auto ValuePlusargsInt(
      std::string_view format, int32_t* output) const -> int32_t;

  // $value$plusargs with string output (%s format).
  // Returns 1 if match found, 0 otherwise. Allocates new string in output.
  [[nodiscard]] auto ValuePlusargsString(
      std::string_view format, std::string* output) const -> int32_t;

  // Register a new monitor, atomically replacing any existing one.
  // Copies initial_prev to runtime-owned buffer.
  void RegisterMonitor(
      MonitorCheckCallback check_thunk, void* design_state,
      const void* initial_prev, uint32_t size);

  // Enable/disable the active monitor. No-op if no active monitor.
  void SetMonitorEnabled(bool enabled);

 private:
  void ExecuteTimeSlot();
  void ExecuteRegion(Region region);
  void ExecutePostponedRegion();

  ProcessRunner runner_;
  SimTime current_time_ = 0;
  bool finished_ = false;
  int8_t global_precision_power_ = -9;   // Set once at simulation init
  common::TimeFormatState time_format_;  // Mutable via $timeformat

  // Time-based scheduling: time → events
  std::map<SimTime, std::vector<ScheduledEvent>> delay_queue_;

  // Region queues for current time slot
  std::vector<ScheduledEvent> active_queue_;
  std::queue<ScheduledEvent> inactive_queue_;

  // Next-delta queue: events scheduled for the next delta cycle
  std::vector<ScheduledEvent> pending_queue_;

  // NBA queue: deferred writes committed in ExecuteRegion(kNBA)
  std::vector<NbaEntry> nba_queue_;

  // Slot ID → base pointer registry (populated lazily via ScheduleNba).
  // Pointers are read-only (used for bit0 snapshot in NBA commit).
  std::unordered_map<uint32_t, const void*> slot_base_ptrs_;

  // Trigger subscriptions: signal → waiting processes
  struct Waiter {
    ProcessHandle handle;
    ResumePoint resume;
    common::EdgeKind edge = common::EdgeKind::kAnyChange;
  };
  std::map<SignalId, std::vector<Waiter>> waiters_;

  // File manager for $fopen/$fclose
  FileManager file_manager_;

  // Postponed queue: callbacks executed at end of time slot ($strobe, etc.)
  // Executed in append order after all delta cycles complete.
  std::vector<PostponedRecord> postponed_queue_;

  // Plusargs for $test$plusargs and $value$plusargs queries.
  std::vector<std::string> plusargs_;

  // Active monitor state ($monitor). Only one can be active at a time.
  // Checked after all strobe callbacks complete in ExecutePostponedRegion.
  std::optional<MonitorState> active_monitor_;
};

}  // namespace lyra::runtime
