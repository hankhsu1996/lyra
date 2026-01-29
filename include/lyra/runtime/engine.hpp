#pragma once

#include <cstddef>
#include <cstdint>
#include <deque>
#include <format>
#include <functional>
#include <map>
#include <optional>
#include <queue>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "lyra/common/edge_kind.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/time_format.hpp"
#include "lyra/runtime/engine_scheduler.hpp"
#include "lyra/runtime/engine_subscriptions.hpp"
#include "lyra/runtime/engine_types.hpp"
#include "lyra/runtime/file_manager.hpp"

namespace lyra::runtime {

// Callback type for process execution.
// Engine calls this when a process should run; callback handles suspension.
// The callback should call Delay/Subscribe/etc. on the engine to reschedule.
using ProcessRunner = std::function<void(
    Engine& engine, ProcessHandle handle, ResumePoint resume)>;

// Simulation Engine: event-driven scheduler for SystemVerilog processes.
//
// Design:
// - Backend-agnostic: both MIR interpreter and LLVM-generated code can use it
// - IEEE 1800 stratified event scheduler (Active -> Inactive -> NBA regions)
// - Processes suspend via Delay/Subscribe, engine resumes them later
//
// Usage:
// 1. Create engine with a ProcessRunner callback
// 2. Schedule initial processes with ScheduleInitial()
// 3. Call Run() to execute until completion or time limit
class Engine {
 public:
  explicit Engine(
      ProcessRunner runner, std::span<const std::string> plusargs = {},
      std::vector<std::string> instance_paths = {})
      : runner_(std::move(runner)),
        plusargs_(plusargs.begin(), plusargs.end()),
        instance_paths_(std::move(instance_paths)) {
  }

  ~Engine() = default;

  // Non-copyable/movable due to intrusive node pointers.
  Engine(const Engine&) = delete;
  auto operator=(const Engine&) -> Engine& = delete;
  Engine(Engine&&) = delete;
  auto operator=(Engine&&) -> Engine& = delete;

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

  // Record a signal update for delta-level coalescing.
  // Edge detection is deferred until FlushSignalUpdates() at delta boundary.
  void RecordSignalUpdate(
      SignalId signal, bool old_lsb, bool new_lsb, bool value_changed);

  // Flush all pending signal updates, waking subscribed processes.
  // Called at delta boundaries (after active/inactive regions, after NBA).
  void FlushSignalUpdates();

  // Run simulation until completion or time limit.
  // Returns final simulation time.
  // By default, runs until the design naturally finishes ($finish, no more
  // events). Pass a specific max_time to cap the simulation.
  auto Run(SimTime max_time = kNoTimeLimit) -> SimTime;

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

  // Get hierarchical path for an instance (%m support).
  // Throws InternalError for invalid instance_id (compiler/runtime bug).
  [[nodiscard]] auto GetInstancePath(uint32_t instance_id) const
      -> std::string_view {
    if (instance_id >= instance_paths_.size()) {
      throw common::InternalError(
          "Engine::GetInstancePath",
          std::format(
              "invalid instance_id {} (have {} instances)", instance_id,
              instance_paths_.size()));
    }
    return instance_paths_[instance_id];
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

  // Subscription management
  void ClearProcessSubscriptions(ProcessHandle handle);
  auto AllocNode() -> SubscriptionNode*;
  void FreeNode(SubscriptionNode* node);

  // Resource limit checking
  auto CheckSubscriptionLimits(const ProcessState& proc_state) -> bool;
  void TerminateWithResourceError(
      std::string_view reason, size_t current, size_t limit);
  void PrintTopWaiters(size_t count);

  // Edge evaluation helpers
  static auto EvaluateEdge(common::EdgeKind edge, bool old_lsb, bool new_lsb)
      -> bool;
  static auto EvaluateEdgeFromRecord(
      common::EdgeKind edge, const EdgeRecord& record) -> bool;

  ProcessRunner runner_;
  SimTime current_time_ = 0;
  bool finished_ = false;
  int8_t global_precision_power_ = -9;   // Set once at simulation init
  common::TimeFormatState time_format_;  // Mutable via $timeformat

  // Time-based scheduling: time -> events
  std::map<SimTime, std::vector<ScheduledEvent>> delay_queue_;

  // Region queues for current time slot
  std::vector<ScheduledEvent> active_queue_;
  std::queue<ScheduledEvent> inactive_queue_;

  // Next-delta queue: events scheduled for the next delta cycle
  std::vector<ScheduledEvent> next_delta_queue_;

  // NBA queue: deferred writes committed in ExecuteRegion(kNBA)
  std::vector<NbaEntry> nba_queue_;

  // Slot ID -> base pointer registry (populated lazily via ScheduleNba).
  // Pointers are read-only (used for bit0 snapshot in NBA commit).
  absl::flat_hash_map<uint32_t, const void*> slot_base_ptrs_;

  // Trigger subscriptions: signal -> waiting processes (intrusive linked lists)
  absl::flat_hash_map<SignalId, SignalWaiters> signal_waiters_;
  absl::flat_hash_map<ProcessHandle, ProcessState, ProcessHandleHash>
      process_states_;

  // Delta-level edge coalescing: signal -> accumulated edges this delta
  absl::flat_hash_map<SignalId, EdgeRecord> pending_edges_;

  // Node pool: deque owns memory (stable pointers), free_list_ tracks reusable
  std::deque<SubscriptionNode> node_pool_;
  std::vector<SubscriptionNode*> free_list_;

  // Resource limits (0 = unlimited)
  size_t live_subscription_count_ = 0;
  size_t max_total_subscriptions_ = 1'000'000;
  size_t max_subscriptions_per_process_ = 10'000;

  // Termination state
  std::optional<std::string> termination_reason_;

  // File manager for $fopen/$fclose
  FileManager file_manager_;

  // Postponed queue: callbacks executed at end of time slot ($strobe, etc.)
  // Executed in append order after all delta cycles complete.
  std::vector<PostponedRecord> postponed_queue_;

  // Plusargs for $test$plusargs and $value$plusargs queries.
  std::vector<std::string> plusargs_;

  // Instance paths for %m support (hierarchical path lookup by instance_id).
  std::vector<std::string> instance_paths_;

  // Active monitor state ($monitor). Only one can be active at a time.
  // Checked after all strobe callbacks complete in ExecutePostponedRegion.
  std::optional<MonitorState> active_monitor_;
};

}  // namespace lyra::runtime
