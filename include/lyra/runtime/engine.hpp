#pragma once

#include <atomic>
#include <cstddef>
#include <cstdint>
#include <deque>
#include <format>
#include <map>
#include <optional>
#include <queue>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "lyra/common/edge_kind.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/mutation_event.hpp"
#include "lyra/common/time_format.hpp"
#include "lyra/runtime/engine_scheduler.hpp"
#include "lyra/runtime/engine_subscriptions.hpp"
#include "lyra/runtime/engine_types.hpp"
#include "lyra/runtime/feature_flags.hpp"
#include "lyra/runtime/file_manager.hpp"
#include "lyra/runtime/loop_site_meta.hpp"
#include "lyra/runtime/process_meta.hpp"
#include "lyra/runtime/slot_meta.hpp"
#include "lyra/runtime/suspend_record.hpp"
#include "lyra/runtime/trap.hpp"
#include "lyra/runtime/update_set.hpp"
#include "lyra/runtime/wait_site.hpp"
#include "lyra/trace/trace_manager.hpp"

namespace lyra::runtime {

// Explicit process dispatch ABI for the hot path.
// fn is required (must not be nullptr). ctx is non-owning and must outlive
// Engine::Run().
using ProcessDispatchFn = void (*)(
    void* ctx, Engine& engine, ProcessHandle handle, ResumePoint resume);

struct ProcessDispatch {
  ProcessDispatchFn fn = nullptr;
  void* ctx = nullptr;
};

// Simulation Engine: event-driven scheduler for SystemVerilog processes.
//
// Design:
// - Backend-agnostic: both MIR interpreter and LLVM-generated code can use it
// - IEEE 1800 stratified event scheduler (Active -> Inactive -> NBA regions)
// - Processes suspend via Delay/Subscribe, engine resumes them later
//
// Usage:
// 1. Create engine with a ProcessDispatch callback
// 2. Schedule initial processes with ScheduleInitial()
// 3. Call Run() to execute until completion or time limit
class Engine {
 public:
  explicit Engine(
      ProcessDispatch process_dispatch, uint32_t num_processes = 0,
      std::span<const std::string> plusargs = {},
      std::vector<std::string> instance_paths = {}, uint32_t feature_flags = 0)
      : process_dispatch_(process_dispatch),
        num_processes_(num_processes),
        process_states_(num_processes),
        plusargs_(plusargs.begin(), plusargs.end()),
        instance_paths_(std::move(instance_paths)),
        feature_flags_(feature_flags) {
    if (process_dispatch_.fn == nullptr) {
      throw common::InternalError(
          "Engine::Engine", "process_dispatch.fn must not be null");
    }
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
  // Returns the created SubscriptionNode, or nullptr if subscription failed.
  auto Subscribe(
      ProcessHandle handle, ResumePoint resume, SignalId signal,
      common::EdgeKind edge) -> SubscriptionNode*;

  // Subscribe with explicit observation byte range within the slot.
  // Returns the created SubscriptionNode, or nullptr if subscription failed.
  auto Subscribe(
      ProcessHandle handle, ResumePoint resume, SignalId signal,
      common::EdgeKind edge, uint32_t byte_offset, uint32_t byte_size,
      uint8_t bit_index = 0) -> SubscriptionNode*;

  // Subscribe to a container (dynamic array/queue) element edge trigger.
  // Chases the handle from DesignState, validates magic, performs OOB check.
  auto SubscribeContainerElement(
      ProcessHandle handle, ResumePoint resume, SignalId signal,
      common::EdgeKind edge, int64_t sv_index, uint32_t elem_stride)
      -> SubscriptionNode*;

  // Create rebind subscriptions for a late-bound edge trigger. For each dep
  // slot, an AnyChange rebind node is created. When any dep changes, the plan
  // is re-evaluated and the edge target's byte_offset/bit_index are updated.
  void SubscribeRebind(
      ProcessHandle handle, ResumePoint resume, SubscriptionNode* target_node,
      std::span<const IndexPlanOp> plan, BitTargetMapping mapping,
      std::span<const uint32_t> dep_slots);

  // Schedule process to resume in the next delta cycle (same time).
  // Used for kRepeat terminator.
  void ScheduleNextDelta(ProcessHandle handle, ResumePoint resume);

  // Enqueue a non-blocking assignment for later commit in the NBA region.
  // mask_ptr == nullptr: full overwrite (direct compare/copy).
  // mask_ptr != nullptr: masked merge (per-byte mask).
  void ScheduleNba(
      void* write_ptr, const void* notify_base_ptr, const void* value_ptr,
      const void* mask_ptr, uint32_t byte_size, uint32_t notify_slot_id);

  // Schedule a callback for the Postponed region ($strobe, future $monitor).
  // Callback executes at end of time slot with final signal values.
  void SchedulePostponed(PostponedCallback callback, void* design_state);

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

  // Get trace manager for event recording.
  [[nodiscard]] auto GetTraceManager() -> trace::TraceManager& {
    return trace_manager_;
  }

  // One-time init for slot metadata registry. Must be called exactly once,
  // before InitConnectionBatch/InitCombKernels (which size from this registry).
  // Sizes signal_waiters_ to the authoritative slot count.
  void InitSlotMeta(SlotMetaRegistry&& registry) {
    if (slot_meta_registry_.IsPopulated()) {
      throw common::InternalError(
          "Engine::InitSlotMeta", "slot meta already initialized");
    }
    std::vector<uint32_t> sizes;
    sizes.reserve(registry.Size());
    for (uint32_t i = 0; i < registry.Size(); ++i) {
      sizes.push_back(registry.Get(i).total_bytes);
    }
    update_set_.Init(registry.Size(), sizes);
    signal_waiters_.resize(registry.Size());
    slot_meta_registry_ = std::move(registry);
  }

  // Set once at simulation init. All process states share the same DesignState
  // pointer (codegen invariant: one DesignState per LyraRunSimulation call).
  void SetDesignStateBase(void* base) {
    design_state_base_ = base;
  }

  // Route a MutationEvent to the UpdateSet (design slots only; heap NYI).
  void OnMutation(const common::MutationEvent& event);

  // Mark slot dirty for scheduler wakeup and deferred trace snapshot.
  // Note: SignalId == slot_id in the current runtime. This equivalence is by
  // design - both identify the same design slot. Callers may pass either type.
  void MarkSlotDirty(uint32_t slot_id) {
    update_set_.MarkSlotDirty(slot_id);
  }

  // Mark a byte range within a slot as dirty.
  void MarkDirtyRange(uint32_t slot_id, uint32_t byte_off, uint32_t byte_size) {
    update_set_.MarkDirtyRange(slot_id, byte_off, byte_size);
  }

  // Mark a heap-relative byte range as dirty for a container slot.
  void MarkExternalDirtyRange(
      uint32_t slot_id, uint32_t byte_off, uint32_t byte_size) {
    update_set_.MarkExternalDirtyRange(slot_id, byte_off, byte_size);
  }

  [[nodiscard]] auto GetSlotMetaRegistry() const -> const SlotMetaRegistry& {
    return slot_meta_registry_;
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

  // Random number generation ($random, $urandom).
  // Advances RNG state on each call.
  [[nodiscard]] auto Random() -> int32_t;
  [[nodiscard]] auto Urandom() -> uint32_t;

  // Feature flag query for optional runtime behaviors ($system, etc.).
  [[nodiscard]] auto IsFeatureEnabled(FeatureFlag flag) const -> bool {
    return HasFlag(static_cast<FeatureFlag>(feature_flags_), flag);
  }

  // Initialize connection batch from descriptors.
  // Builds trigger map for fast evaluation.
  void InitConnectionBatch(std::span<const ConnectionDescriptor> descs);

  // Evaluate all connections once (used for initial value propagation).
  void EvaluateAllConnections();

  // Initialize comb kernels from word table and comb function array.
  // Parses trigger metadata, builds trigger map.
  // comb_funcs: one void-returning entry per comb kernel, in word-table order.
  // states: full process state array, indexed by proc_idx from word table.
  // Comb kernels do not participate in the process trap/return-code contract.
  using CombFunc = void (*)(void*, uint32_t);
  void InitCombKernels(
      std::span<const uint32_t> words, CombFunc* comb_funcs, void** states);

  // Mark all comb kernel trigger slots dirty to ensure initial evaluation.
  void SeedCombKernelDirtyMarks();

  // Flush signal updates + evaluate triggered connections/comb kernels until
  // convergence. Also used for initial value propagation before Run().
  void FlushAndPropagateConnections();

  // One-time init for process metadata registry.
  void InitProcessMeta(ProcessMetaRegistry registry) {
    process_meta_ = std::move(registry);
  }

  // One-time init for loop site metadata registry.
  void InitLoopSiteMeta(LoopSiteRegistry registry) {
    loop_site_meta_ = std::move(registry);
  }

  // One-time init for wait-site metadata registry.
  void InitWaitSiteMeta(WaitSiteRegistry registry) {
    wait_site_meta_ = std::move(registry);
  }

  // Register suspend record pointers for post-activation reconciliation.
  void RegisterSuspendRecords(std::span<SuspendRecord*> records);

  // Single source of truth for whether the engine uses post-activation
  // reconciliation (new path) vs legacy HandleSuspendRecord (old path).
  // True when both wait-site metadata and suspend-record access are present.
  [[nodiscard]] auto HasPostActivationReconciliation() const -> bool {
    return wait_site_meta_.IsPopulated() && !suspend_records_.empty();
  }

  // Post-activation reconciliation: engine-owned dispatch after process runs.
  void ReconcilePostActivation(ProcessHandle handle);

  [[nodiscard]] auto GetProcessMetaRegistry() const
      -> const ProcessMetaRegistry& {
    return process_meta_;
  }

  [[nodiscard]] auto GetLoopSiteRegistry() const -> const LoopSiteRegistry& {
    return loop_site_meta_;
  }

  // Format process identity for diagnostics (normal code path).
  [[nodiscard]] auto FormatProcess(uint32_t process_id) const -> std::string;

  // Async-signal-safe dump of scheduler status to fd.
  // Prints: phase, sim_time, activation_seq, current/last process.
  void DumpSchedulerStatusAsyncSignalSafe(int fd) const;

  // Get current running process ID.
  [[nodiscard]] auto CurrentRunningProcessId() const -> uint32_t {
    return current_running_process_.load(std::memory_order_acquire);
  }

  // Handle a trap raised by generated code (loop budget exceeded, etc.).
  void HandleTrap(uint32_t process_id, const TrapPayload& payload);

  // Scheduler phase for signal-safe status dump.
  enum class Phase : uint32_t {
    kIdle,
    kAdvanceTime,
    kRunProcess,
    kFlushUpdates,
    kCommitNba,
    kPostponed,
  };

 private:
  // Run a single process activation.
  // Establishes clean activation-entry state (trap reset, loop budget),
  // then invokes the runner which owns exit-status interpretation.
  void RunOneActivation(const ScheduledEvent& event);

  void ExecuteTimeSlot();
  void ExecuteRegion(Region region);
  void ExecutePostponedRegion();
  void FlushDirtySlots();

  // Subscription lifecycle
  void ClearInstalledSubscriptions(ProcessHandle handle);
  void InvalidateInstalledWait(ProcessHandle handle);
  void ResetInstalledWait(ProcessHandle handle);
  void ClearProcessSubscriptions(ProcessHandle handle);
  auto AllocNode() -> SubscriptionNode*;
  void FreeNode(SubscriptionNode* node);

  // Persistent wait-site installation
  void InstallWaitSite(
      ProcessHandle handle, SuspendRecord* suspend,
      const CompiledWaitSite& descriptor);
  void RefreshInstalledSnapshots(ProcessHandle handle);

  // Late-bound rebinding: re-read index value, recompute edge target.
  void RebindSubscription(SubscriptionNode* rebind_node);

  // Resource limit checking
  auto CheckSubscriptionLimits(const ProcessState& proc_state) -> bool;
  void TerminateWithResourceError(
      std::string_view reason, size_t current, size_t limit);
  void PrintTopWaiters(size_t count);

  // Edge evaluation helpers
  static auto EvaluateEdge(common::EdgeKind edge, bool old_lsb, bool new_lsb)
      -> bool;

  ProcessDispatch process_dispatch_;
  uint32_t num_processes_ = 0;
  std::vector<ProcessState> process_states_;
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

  // Dense signal waiter table (indexed by slot_id, sized in InitSlotMeta).
  std::vector<SignalWaiters> signal_waiters_;

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

  // Flush epoch: incremented at start of each FlushSignalUpdates.
  // Used by rebind epoch guard to avoid redundant re-evaluations.
  uint32_t flush_epoch_ = 1;

  // PRNG state for $random/$urandom. LCG with glibc constants.
  // Initial seed = 1 for deterministic reproducibility.
  uint32_t prng_state_ = 1;

  // Feature flags for optional runtime behaviors (see FeatureFlag enum).
  uint32_t feature_flags_ = 0;

  // Slot metadata registry (empty until populated by JIT codegen).
  SlotMetaRegistry slot_meta_registry_;

  // Trace event manager (disabled by default, zero overhead when off).
  trace::TraceManager trace_manager_;

  // Shared dirty tracking for scheduler wakeup and trace snapshots.
  // design_state_base_: base pointer for DesignState (set once at init).
  // update_set_: tracks dirty slots (per-delta for scheduler, per-time-slot
  // for trace).
  void* design_state_base_ = nullptr;
  UpdateSet update_set_;

  // Connection batch: fast-path for kernelized connection processes.
  // Instead of scheduling connection processes through the full engine,
  // connections are evaluated inline during signal propagation.
  struct BatchedConnection {
    uint32_t src_byte_offset;
    uint32_t dst_byte_offset;
    uint32_t byte_size;
    uint32_t dst_slot_id;
  };
  // All batched connections (for initial evaluation).
  std::vector<BatchedConnection> all_connections_;
  // Dense trigger range table (indexed by slot_id, sized from slot registry).
  std::vector<TriggerRange> conn_trigger_map_;

  // Comb kernel batch: pure combinational processes evaluated inline.
  // Each kernel has a compiled function pointer and state pointer.
  struct CombKernel {
    CombFunc func;
    void* state;
    uint32_t process_index;  // Original index in processes array (for skip)
  };
  std::vector<CombKernel> comb_kernels_;
  // Flat backing array of kernel indices, indexed via comb_trigger_map_ ranges.
  std::vector<uint32_t> comb_trigger_backing_;
  // Dense per-slot range table into comb_trigger_backing_ (sized from slot
  // registry).
  std::vector<TriggerRange> comb_trigger_map_;
  // List of trigger slots for seeding dirty marks.
  std::vector<uint32_t> comb_trigger_slots_;
  // Dense flag table by process index (sized from num_processes_ in
  // InitCombKernels, true = comb kernel, skip in ScheduleInitial).
  std::vector<uint8_t> comb_kernel_flags_;

  // Process metadata registry for diagnostics and signal-safe dumps.
  ProcessMetaRegistry process_meta_;

  // Loop site metadata registry for loop guard diagnostics.
  LoopSiteRegistry loop_site_meta_;

  // Wait-site metadata registry for persistent wait installation.
  WaitSiteRegistry wait_site_meta_;

  // Suspend record access for post-activation reconciliation.
  std::vector<SuspendRecord*> suspend_records_;

  // Scheduler observability atomics (signal-safe reads from SIGUSR1).
  // Written by scheduler, read by signal handler via relaxed loads.
  std::atomic<uint32_t> current_running_process_{UINT32_MAX};
  std::atomic<uint32_t> last_process_id_{UINT32_MAX};
  std::atomic<uint64_t> activation_seq_{0};
  std::atomic<uint32_t> phase_{static_cast<uint32_t>(Phase::kIdle)};
};

}  // namespace lyra::runtime
