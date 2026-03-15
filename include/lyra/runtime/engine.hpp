#pragma once

#include <atomic>
#include <cstddef>
#include <cstdint>
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
#include "lyra/runtime/activation_trace.hpp"
#include "lyra/runtime/back_edge_site_meta.hpp"
#include "lyra/runtime/engine_scheduler.hpp"
#include "lyra/runtime/engine_subscriptions.hpp"
#include "lyra/runtime/engine_types.hpp"
#include "lyra/runtime/feature_flags.hpp"
#include "lyra/runtime/file_manager.hpp"
#include "lyra/runtime/observer.hpp"
#include "lyra/runtime/process_meta.hpp"
#include "lyra/runtime/slot_meta.hpp"
#include "lyra/runtime/suspend_record.hpp"
#include "lyra/runtime/trace_selection.hpp"
#include "lyra/runtime/trace_signal_meta.hpp"
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

// Always-on summary counters: one increment per major event.
struct CoreRuntimeStats {
  uint64_t total_activations = 0;
  // Activations where the process body made no direct dirty marks (only
  // scheduled NBAs or read-only). Not a wasted-wakeup metric: always_ff
  // processes legitimately schedule NBAs without marking slots dirty.
  uint64_t activations_nba_only = 0;
  uint64_t propagation_calls = 0;
  uint64_t propagation_iterations = 0;
  uint64_t propagation_max_iterations = 0;
  uint64_t nba_entries = 0;
  uint64_t nba_elided = 0;
  uint64_t nba_changed = 0;
};

// Opt-in per-element counters: collected only when kDetailedStats is enabled.
struct DetailedRuntimeStats {
  uint64_t dirty_mark_calls = 0;
  uint64_t flush_dirty_slots = 0;
  uint64_t conn_considered = 0;
  uint64_t conn_memcmp_executed = 0;
  uint64_t conn_memcpy_executed = 0;
  uint64_t comb_considered = 0;
  uint64_t comb_executed = 0;
  uint64_t comb_skipped_range = 0;
  uint64_t edge_sub_checks = 0;
  uint64_t edge_sub_wakeups = 0;
  uint64_t change_sub_checks = 0;
  uint64_t change_sub_wakeups = 0;
  uint64_t wakeup_attempts = 0;
  uint64_t wakeup_deduped = 0;
};

// Composite runtime stats: core (always-on) + detailed (opt-in).
struct RuntimeStats {
  CoreRuntimeStats core;
  DetailedRuntimeStats detailed;
};

// Simulation Engine: event-driven scheduler for SystemVerilog processes.
//
// Design:
// - Backend-agnostic: decoupled from code generation
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
    // Pre-reserve wakeup queue to avoid reallocation during flush.
    // Each process can be enqueued at most once per delta (dedup by
    // is_enqueued), so num_processes is a tight upper bound.
    next_delta_queue_.reserve(num_processes);
    active_queue_.reserve(num_processes);
    if (HasFlag(
            static_cast<FeatureFlag>(feature_flags_),
            FeatureFlag::kEnableActivationTrace)) {
      activation_trace_.emplace();
      wake_trace_.resize(num_processes);
    }
    detailed_stats_enabled_ = HasFlag(
        static_cast<FeatureFlag>(feature_flags_), FeatureFlag::kDetailedStats);
  }

  ~Engine() = default;

  // Non-copyable/movable due to internal subscription storage.
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
  // Returns the index of the created subscription in the slot's typed vector,
  // or UINT32_MAX if subscription failed.
  // initially_active: if false, subscription is created in inactive state.
  auto Subscribe(
      ProcessHandle handle, ResumePoint resume, SignalId signal,
      common::EdgeKind edge, bool initially_active = true) -> uint32_t;

  // Subscribe with explicit observation byte range within the slot.
  // Returns the index of the created subscription in the slot's typed vector,
  // or UINT32_MAX if subscription failed.
  // initially_active: if false, subscription is created in inactive state.
  auto Subscribe(
      ProcessHandle handle, ResumePoint resume, SignalId signal,
      common::EdgeKind edge, uint32_t byte_offset, uint32_t byte_size,
      uint8_t bit_index, bool initially_active = true) -> uint32_t;

  // Subscribe to a container (dynamic array/queue) element edge trigger.
  // Chases the handle from DesignState, validates magic, performs OOB check.
  // initially_active: codegen's descriptor-time classification. If false,
  // the subscription is created inactive and runtime skips validation. If
  // true, runtime still validates handle/bounds and may downgrade to inactive.
  auto SubscribeContainerElement(
      ProcessHandle handle, ResumePoint resume, SignalId signal,
      common::EdgeKind edge, int64_t sv_index, uint32_t elem_stride,
      bool initially_active = true) -> uint32_t;

  // Canonical trigger installation from typed descriptors.
  // Installs subscriptions directly from per-trigger metadata (kind, flags,
  // container_elem_stride), then hooks up rebind watchers from late-bound
  // headers. Both InstallWaitSite and HandleSuspendRecord delegate here.
  void InstallTriggers(
      ProcessHandle handle, ResumePoint resume,
      std::span<const WaitTriggerRecord> triggers,
      std::span<const LateBoundHeader> late_bound,
      std::span<const IndexPlanOp> plan_ops,
      std::span<const uint32_t> dep_slots);

  // Create rebind subscriptions for a late-bound edge trigger. For each dep
  // slot, an AnyChange rebind node is created. When any dep changes, the plan
  // is re-evaluated and the edge target's observation position is updated.
  // edge_target_id identifies the target in edge_target_table_.
  // target_kind must be kEdge or kContainer (kChange not supported).
  //
  // Atomicity: either all dep_slot watchers are installed, or none are.
  // Pre-validates capacity for all watchers before mutating any state.
  // Validates target_slot and target_index against current dense storage.
  void SubscribeRebind(
      ProcessHandle handle, uint32_t edge_target_id, SignalId target_slot,
      SubKind target_kind, uint32_t target_index, uint8_t target_edge_group,
      EdgeBucket target_edge_bucket, std::span<const IndexPlanOp> plan,
      BitTargetMapping mapping, std::span<const uint32_t> dep_slots);

  // Schedule process to resume in the next delta cycle (same time).
  // Used for kRepeat terminator.
  void ScheduleNextDelta(ProcessHandle handle, ResumePoint resume);

  // Enqueue a non-blocking assignment for later commit in the NBA region.
  // mask_ptr == nullptr: full overwrite (direct compare/copy).
  // mask_ptr != nullptr: masked merge (per-byte mask).
  void ScheduleNba(
      void* write_ptr, const void* notify_base_ptr, const void* value_ptr,
      const void* mask_ptr, uint32_t byte_size, uint32_t notify_slot_id);

  // Register a strobe observer for the Postponed region.
  // Executes at end of time slot with final signal values.
  void RegisterStrobe(
      StrobeProgramFn program, void* design_state,
      const ObserverContext& context);

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

  // Runtime counters accumulated during simulation.
  [[nodiscard]] auto GetStats() const -> const RuntimeStats& {
    return stats_;
  }

  // Print runtime stats (propagation + scheduler) to sink.
  void DumpRuntimeStats(FILE* sink) const;

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
  // Sizes signal_subs_ to the authoritative slot count.
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
    signal_subs_.resize(registry.Size());
    activation_slot_gen_.resize(registry.Size(), 0);
    slot_meta_registry_ = std::move(registry);
  }

  // Set once at simulation init. All process states share the same DesignState
  // pointer (codegen invariant: one DesignState per LyraRunSimulation call).
  void SetDesignStateBase(void* base) {
    design_state_base_ = base;
  }

  // Route a MutationEvent to the UpdateSet (design slots only; heap NYI).
  void OnMutation(const common::MutationEvent& event);

  // Whether the inline first-dirty fast path is allowed in the current
  // execution context. Returns false during comb-fixpoint evaluation, where
  // repeated dirty marks carry semantic work (comb_write_capture_ bookkeeping
  // for convergence). Returns true during normal process-body execution.
  [[nodiscard]] auto IsFirstDirtyFastPathAllowed() const -> bool {
    return comb_write_capture_ == nullptr;
  }

  // Pointer to the per-delta first-dirty seen bitmap. Pure data accessor;
  // does not encode execution-policy decisions. Returns null if the update
  // set is not yet initialized.
  auto GetFirstDirtySeenPtr() -> uint8_t* {
    return update_set_.DeltaSeenData();
  }

  // First-dirty slow path: called by generated code when the inline guard
  // detects a slot not yet dirty in the current delta. Performs all first-
  // dirty bookkeeping (dedup tracking, activation dirty count, stats).
  // Precondition: the inline guard already confirmed delta_seen[slot] == 0.
  void MarkSlotDirtyFirst(uint32_t slot_id) {
    if (detailed_stats_enabled_) ++stats_.detailed.dirty_mark_calls;
    update_set_.MarkSlotDirty(slot_id);
    if (activation_ctx_.active) {
      NoteActivationDirty(slot_id);
    }
  }

  // Mark slot dirty for scheduler wakeup and deferred trace snapshot.
  // Note: SignalId == slot_id in the current runtime. This equivalence is by
  // design - both identify the same design slot. Callers may pass either type.
  void MarkSlotDirty(uint32_t slot_id) {
    if (detailed_stats_enabled_) ++stats_.detailed.dirty_mark_calls;
    bool first_dirty = update_set_.MarkSlotDirty(slot_id);
    if (comb_write_capture_ != nullptr) {
      comb_write_capture_->push_back(slot_id);
    }
    if (first_dirty && activation_ctx_.active) {
      NoteActivationDirty(slot_id);
    }
  }

  // Mark a byte range within a slot as dirty.
  void MarkDirtyRange(uint32_t slot_id, uint32_t byte_off, uint32_t byte_size) {
    if (detailed_stats_enabled_) ++stats_.detailed.dirty_mark_calls;
    update_set_.MarkDirtyRange(slot_id, byte_off, byte_size);
    if (comb_write_capture_ != nullptr) {
      comb_write_capture_->push_back(slot_id);
    }
    if (activation_ctx_.active) {
      NoteActivationDirty(slot_id);
    }
  }

  // Mark a heap-relative byte range as dirty for a container slot.
  // Intentionally excluded from comb write capture: heap/container mutations
  // do not participate in flat connection/comb convergence. Connections are
  // byte-range copies between design slots, and comb kernels read/write
  // packed design slots only.
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
      MonitorCheckProgramFn program, void* design_state,
      const ObserverContext& context, const void* initial_prev, uint32_t size);

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

  // One-time init for back-edge site metadata registry.
  void InitBackEdgeSiteMeta(BackEdgeSiteRegistry registry) {
    back_edge_site_meta_ = std::move(registry);
  }

  // One-time init for wait-site metadata registry.
  void InitWaitSiteMeta(WaitSiteRegistry registry) {
    wait_site_meta_ = std::move(registry);
  }

  // One-time init for trace signal metadata registry. Must be called exactly
  // once, after InitSlotMeta. Engine owns both the registry and the selection
  // mask. The non-owning pointer passed to TraceManager remains valid for
  // Engine's lifetime because trace_signal_meta_ is never moved after this
  // call. Validates that signal count matches slot count (compile-time
  // invariant: one trace signal per design slot).
  void InitTraceSignalMeta(TraceSignalMetaRegistry registry) {
    if (trace_signal_meta_.IsPopulated()) {
      throw common::InternalError(
          "Engine::InitTraceSignalMeta",
          "trace signal metadata already initialized");
    }
    auto signal_count = static_cast<uint32_t>(registry.Count());
    if (slot_meta_registry_.IsPopulated() &&
        signal_count != slot_meta_registry_.Size()) {
      throw common::InternalError(
          "Engine::InitTraceSignalMeta",
          std::format(
              "signal count {} does not match slot count {}", signal_count,
              slot_meta_registry_.Size()));
    }
    trace_signal_meta_ = std::move(registry);
    trace_manager_.SetSignalMeta(&trace_signal_meta_);
    trace_selection_.Init(signal_count);
  }

  [[nodiscard]] auto GetTraceSignalMetaRegistry() const
      -> const TraceSignalMetaRegistry& {
    return trace_signal_meta_;
  }

  [[nodiscard]] auto GetTraceSelection() const
      -> const TraceSelectionRegistry& {
    return trace_selection_;
  }

  auto GetTraceSelection() -> TraceSelectionRegistry& {
    return trace_selection_;
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

  [[nodiscard]] auto GetBackEdgeSiteRegistry() const
      -> const BackEdgeSiteRegistry& {
    return back_edge_site_meta_;
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
  void RunOneActivation(const WakeupEntry& entry);

  void ExecuteTimeSlot();
  void ExecuteRegion(Region region);
  void ExecuteActiveRegion();
  void ExecuteInactiveRegion();
  void ExecuteNbaRegion();
  void ExecutePostponedRegion();
  void FlushDirtySlots();

  // Subscription lifecycle
  void ClearInstalledSubscriptions(ProcessHandle handle);
  void InvalidateInstalledWait(ProcessHandle handle);
  void ResetInstalledWait(ProcessHandle handle);
  void ClearProcessSubscriptions(ProcessHandle handle);

  // Typed cold pool management.
  auto AllocEdgeCold() -> uint32_t;
  void FreeEdgeCold(uint32_t idx);
  auto AllocChangeCold() -> uint32_t;
  void FreeChangeCold(uint32_t idx);
  auto AllocWatcherCold() -> uint32_t;
  void FreeWatcherCold(uint32_t idx);
  auto AllocContainerCold() -> uint32_t;
  void FreeContainerCold(uint32_t idx);

  // Edge target table management.
  auto AllocEdgeTarget(EdgeTargetHandle handle) -> uint32_t;
  void FreeEdgeTarget(uint32_t id);

  // Grouped edge subscription accessors.
  auto GetEdgeGroup(uint32_t slot_id, uint32_t group) -> EdgeWatchGroup&;
  auto EdgeSubVec(uint32_t slot_id, uint32_t group, EdgeBucket bucket)
      -> std::vector<EdgeSub>&;
  auto ResolveEdgeSub(const SubRef& ref) -> EdgeSub&;

  // Find or create an EdgeWatchGroup for the given observation point.
  // Returns the group index. Reuses empty groups before appending.
  auto FindOrCreateEdgeGroup(
      uint32_t slot_id, uint32_t byte_offset, uint8_t bit_index,
      uint8_t initial_last_bit) -> uint32_t;

  // Low-level edge sub removal from a specific group/bucket.
  // Handles swap-and-pop with SubRef and EdgeTargetHandle fixup.
  void RemoveEdgeSubFromBucket(
      uint32_t slot_id, uint32_t group, EdgeBucket bucket, uint32_t index);

  // Typed swap-and-pop removal from dense vectors.
  void RemoveEdgeSub(const SubRef& ref);
  void RemoveChangeSub(uint32_t slot_id, uint32_t index);
  void RemoveRebindWatcherSub(uint32_t slot_id, uint32_t index);
  void RemoveContainerSub(uint32_t slot_id, uint32_t index);

  // Persistent wait-site installation
  void InstallWaitSite(
      ProcessHandle handle, SuspendRecord* suspend,
      const CompiledWaitSite& descriptor);
  void RefreshInstalledSnapshots(ProcessHandle handle);

  // Late-bound rebinding: re-read index value, recompute edge target.
  void RebindSubscription(uint32_t edge_target_id);

  // Per-dirty-slot flush: ordered dispatch into per-kind helpers.
  void FlushDirtySlot(
      uint32_t slot_id, SlotSubscriptions& slot, const SlotMeta& meta,
      std::span<const uint8_t> design_state);

  // Activation-local dirty dedup for design-slot dirties only.
  // Called from MarkSlotDirty and MarkDirtyRange. Excludes
  // MarkExternalDirtyRange (heap/container) by design.
  void NoteActivationDirty(uint32_t slot_id);

  // Trace helpers: append event + live stderr print.
  // TraceWake is called at the single point where an activation becomes
  // runnable (ExecuteRegion kActive), not at producer-side queue insertion.
  // Reads trace-only fields (cause, trigger_slot) from wake_trace_.
  void TraceWake(const WakeupEntry& entry);
  void TraceRun(const WakeupEntry& entry);

  // Per-kind flush helpers (called from FlushDirtySlot).
  void FlushSlotRebindSubs(
      std::vector<RebindWatcherSub>& subs, const SlotMeta& meta,
      std::span<const uint8_t> design_state);
  void FlushSlotEdgeGroups(
      uint32_t slot_id, std::vector<EdgeWatchGroup>& groups,
      const SlotMeta& meta, const common::RangeSet& dirty_ranges,
      RangeFilterMode mode, std::span<const uint8_t> design_state);
  void UpdateEdgeColdSnapshots(EdgeWatchGroup& group, uint8_t current_byte);
  void FlushSlotChangeSubs(
      uint32_t slot_id, std::vector<ChangeSub>& subs, const SlotMeta& meta,
      const common::RangeSet& dirty_ranges, RangeFilterMode mode,
      std::span<const uint8_t> design_state);
  void FlushSlotContainerSubs(
      uint32_t slot_id, std::vector<ContainerSub>& subs,
      std::span<const uint8_t> design_state);

  // Single container sub flush.
  void FlushContainerSub(
      uint32_t slot_id, ContainerSub& sub,
      std::span<const uint8_t> design_state);

  // Deduplicated wakeup enqueue: push to next_delta_queue_ if not already
  // enqueued. Shared by edge, change, and container flush paths.
  //
  // Fully inline: the hot path (already enqueued) is a single load + branch.
  // The cold path (first enqueue) constructs a 12-byte WakeupEntry and
  // pushes to the pre-reserved queue. Trace-only fields (cause, trigger_slot)
  // are stored per-process only when activation tracing is enabled.
  void EnqueueProcessWakeup(
      uint32_t process_id, uint32_t instance_id, uint32_t resume_block,
      uint32_t trigger_slot, WakeCause cause) {
    if (detailed_stats_enabled_) ++stats_.detailed.wakeup_attempts;
    if (process_states_[process_id].is_enqueued) {
      if (detailed_stats_enabled_) ++stats_.detailed.wakeup_deduped;
      return;
    }
    next_delta_queue_.push_back({process_id, instance_id, resume_block});
    process_states_[process_id].is_enqueued = true;
    if (activation_trace_.has_value()) {
      wake_trace_[process_id] = {.cause = cause, .trigger_slot = trigger_slot};
    }
  }

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
  std::map<SimTime, std::vector<WakeupEntry>> delay_queue_;

  // Region queues for current time slot
  std::vector<WakeupEntry> active_queue_;
  std::queue<WakeupEntry> inactive_queue_;

  // Next-delta queue: events scheduled for the next delta cycle
  std::vector<WakeupEntry> next_delta_queue_;

  // Per-process trace annotations (only populated when tracing enabled).
  // Indexed by process_id. Safe because each process is enqueued at most
  // once per delta (dedup guard prevents overwrite).
  std::vector<WakeTraceInfo> wake_trace_;

  // NBA queue: deferred writes committed in ExecuteRegion(kNBA)
  std::vector<NbaEntry> nba_queue_;

  // Dense per-slot subscription storage (indexed by slot_id, sized in
  // InitSlotMeta). Four typed vectors per slot for branch-free flush scans.
  std::vector<SlotSubscriptions> signal_subs_;

  // Typed cold pools: each sub kind has its own cold storage.
  std::vector<EdgeTargetCold> edge_cold_pool_;
  std::vector<uint32_t> edge_cold_free_list_;
  std::vector<ChangeSnapshotCold> change_cold_pool_;
  std::vector<uint32_t> change_cold_free_list_;
  std::vector<WatcherCold> watcher_cold_pool_;
  std::vector<uint32_t> watcher_cold_free_list_;
  std::vector<ContainerCold> container_cold_pool_;
  std::vector<uint32_t> container_cold_free_list_;

  // Edge target table: stable indirection for rebind targets.
  // Indexed by edge_target_id, updated on swap-and-pop of the target.
  std::vector<EdgeTargetHandle> edge_target_table_;
  std::vector<uint32_t> edge_target_free_list_;

  // Resource limits (0 = unlimited)
  size_t live_subscription_count_ = 0;
  size_t max_total_subscriptions_ = 1'000'000;
  size_t max_subscriptions_per_process_ = 10'000;

  // Termination state
  std::optional<std::string> termination_reason_;

  // File manager for $fopen/$fclose
  FileManager file_manager_;

  // Strobe observer queue: executed at end of time slot ($strobe, etc.)
  // Executed in append order after all delta cycles complete.
  std::vector<StrobeRecord> postponed_queue_;

  // Plusargs for $test$plusargs and $value$plusargs queries.
  std::vector<std::string> plusargs_;

  // Instance paths for %m support (hierarchical path lookup by instance_id).
  std::vector<std::string> instance_paths_;

  // Active monitor state ($monitor). Only one can be active at a time.
  // Checked after all strobe callbacks complete in ExecutePostponedRegion.
  std::optional<MonitorRecord> active_monitor_;

  // Flush epoch: incremented at start of each FlushSignalUpdates.
  // Used by rebind epoch guard to avoid redundant re-evaluations.
  uint32_t flush_epoch_ = 1;

  // PRNG state for $random/$urandom. LCG with glibc constants.
  // Initial seed = 1 for deterministic reproducibility.
  uint32_t prng_state_ = 1;

  // Feature flags for optional runtime behaviors (see FeatureFlag enum).
  uint32_t feature_flags_ = 0;

  // Cached from kDetailedStats feature flag at construction.
  bool detailed_stats_enabled_ = false;

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
    uint32_t flags;          // Bit 0 = kSelfEdge
    static constexpr uint32_t kSelfEdge = 1U;
  };
  std::vector<CombKernel> comb_kernels_;
  // Structured trigger entries with byte-range observation.
  struct CombTriggerEntry {
    uint32_t kernel_idx;
    uint32_t byte_offset;
    uint32_t byte_size;  // 0 = full-slot
    // Runtime acceleration copy; source of truth is CombKernel::flags.
    bool has_self_edge;
  };
  std::vector<CombTriggerEntry> comb_trigger_backing_;
  // Dense per-slot range table into comb_trigger_backing_ (sized from slot
  // registry).
  std::vector<TriggerRange> comb_trigger_map_;
  // List of trigger slots for seeding dirty marks.
  std::vector<uint32_t> comb_trigger_slots_;
  // Dense flag table by process index (sized from num_processes_ in
  // InitCombKernels, true = comb kernel, skip in ScheduleInitial).
  std::vector<uint8_t> comb_kernel_flags_;

  // Current delta cycle within the active time slot. Reset to 0 at the
  // start of each ExecuteTimeSlot. Part of the runtime execution model:
  // (current_time_, current_delta_) together identify the scheduling epoch.
  uint32_t current_delta_ = 0;

  // Activation trace: bounded ring buffer of structured activation events
  // with live stderr output. Only allocated when kEnableActivationTrace is set.
  std::optional<ActivationTrace> activation_trace_;

  // Per-activation run context for dirty counting.
  struct ActivationRunContext {
    bool active = false;
    uint32_t generation = 0;
    uint32_t dirty_count = 0;
  };
  ActivationRunContext activation_ctx_;

  // Per-slot generation vector for activation-local dirty dedup.
  // Sized in InitSlotMeta.
  std::vector<uint32_t> activation_slot_gen_;

  // Comb write capture for FlushAndPropagateConnections fixed-point loop.
  // When non-null, MarkSlotDirty/MarkDirtyRange additionally push to this
  // vector to bypass delta_seen_ dedup. Owned by a CombWriteGuard on the
  // stack of FlushAndPropagateConnections; null at all other times.
  std::vector<uint32_t>* comb_write_capture_ = nullptr;

  // Process metadata registry for diagnostics and signal-safe dumps.
  ProcessMetaRegistry process_meta_;

  // Back-edge site metadata registry for iteration limit diagnostics.
  BackEdgeSiteRegistry back_edge_site_meta_;

  // Wait-site metadata registry for persistent wait installation.
  WaitSiteRegistry wait_site_meta_;

  // Trace signal metadata registry (empty until populated).
  TraceSignalMetaRegistry trace_signal_meta_;

  // Per-slot selection mask for producer-side trace filtering.
  // Initialized alongside trace_signal_meta_ in InitTraceSignalMeta().
  TraceSelectionRegistry trace_selection_;

  // Suspend record access for post-activation reconciliation.
  std::vector<SuspendRecord*> suspend_records_;

  // Scheduler observability atomics (signal-safe reads from SIGUSR1).
  // Written by scheduler, read by signal handler via relaxed loads.
  std::atomic<uint32_t> current_running_process_{UINT32_MAX};
  std::atomic<uint32_t> last_process_id_{UINT32_MAX};
  std::atomic<uint64_t> activation_seq_{0};
  std::atomic<uint32_t> phase_{static_cast<uint32_t>(Phase::kIdle)};

  // Execution-discipline counters (accumulated during Run).
  RuntimeStats stats_;

  // Static connection batch shape (populated once in InitConnectionBatch).
  uint32_t conn_full_slot_count_ = 0;
  uint32_t conn_narrow_count_ = 0;

  // Static comb trigger batch shape (populated once in InitCombKernels).
  uint32_t comb_full_slot_count_ = 0;
  uint32_t comb_narrow_count_ = 0;

  // True if any comb kernel has self-edge risk. Controls whether snapshot
  // infrastructure is allocated in FlushAndPropagateConnections.
  bool has_any_self_edge_comb_ = false;

  // Pre-comb snapshot descriptor for self-trigger suppression.
  struct CombSnapshot {
    uint32_t buf_off;
    uint32_t base_off;
    uint32_t total_bytes;
  };

  // Fixpoint workspace for FlushAndPropagateConnections.
  // Persistent engine-owned scratch storage, reused across invocations.
  // All vectors are transient -- they carry no semantic state across calls.
  //
  // Allocation invariants:
  // - Slot-count-sized vectors (pending_seen, snapshot_index) are resized
  //   from the runtime slot count, either in InitCombKernels or lazily on
  //   first FlushAndPropagateConnections call. No code assumes
  //   InitCombKernels has run first (connection-only designs skip it).
  // - Worklists (pending, next_pending, comb_writes) start empty and grow
  //   on demand. clear() preserves capacity, so after warmup there are no
  //   further allocations.
  // - Snapshot vectors (snapshot_buf, snapshots, snapshotted_slots) are only
  //   used when has_any_self_edge_comb_ is true. Same clear()-preserves-
  //   capacity pattern.
  struct FixpointWorkspace {
    std::vector<uint32_t> pending;
    std::vector<uint32_t> next_pending;
    std::vector<uint32_t> comb_writes;
    std::vector<uint8_t> pending_seen;
    std::vector<uint32_t> snapshot_index;
    std::vector<uint8_t> snapshot_buf;
    std::vector<CombSnapshot> snapshots;
    std::vector<uint32_t> snapshotted_slots;
  };
  FixpointWorkspace fp_work_;
};

}  // namespace lyra::runtime
