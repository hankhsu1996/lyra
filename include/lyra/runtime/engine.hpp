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
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

#include "lyra/common/deferred_assertion_abi.hpp"
#include "lyra/common/edge_kind.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/mutation_event.hpp"
#include "lyra/common/time_format.hpp"
#include "lyra/runtime/activation_trace.hpp"
#include "lyra/runtime/back_edge_site_meta.hpp"
#include "lyra/runtime/decision.hpp"
#include "lyra/runtime/engine_scheduler.hpp"
#include "lyra/runtime/engine_subscriptions.hpp"
#include "lyra/runtime/engine_types.hpp"
#include "lyra/runtime/event_registry.hpp"
#include "lyra/runtime/feature_flags.hpp"
#include "lyra/runtime/file_manager.hpp"
#include "lyra/runtime/instance_metadata.hpp"
#include "lyra/runtime/instance_observability.hpp"
#include "lyra/runtime/observer.hpp"
#include "lyra/runtime/process_frame.hpp"
#include "lyra/runtime/process_meta.hpp"
#include "lyra/runtime/process_trigger_registry.hpp"
#include "lyra/runtime/reporting.hpp"
#include "lyra/runtime/scheduler_snapshot.hpp"
#include "lyra/runtime/signal_coord.hpp"
#include "lyra/runtime/slot_meta.hpp"
#include "lyra/runtime/small_byte_buffer.hpp"
#include "lyra/runtime/suspend_record.hpp"
#include "lyra/runtime/trace_selection.hpp"
#include "lyra/runtime/trace_signal_meta.hpp"
#include "lyra/runtime/trap.hpp"
#include "lyra/runtime/update_set.hpp"
#include "lyra/runtime/wait_site.hpp"
#include "lyra/trace/instance_trace_resolver.hpp"
#include "lyra/trace/trace_manager.hpp"
#include "svdpi.h"

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

  // MarkSlotDirty cost breakdown.
  uint64_t dirty_mark_fast_path = 0;
  uint64_t dirty_mark_first_touch = 0;

  // Worklist propagation internals.
  uint64_t prop_pending_slots = 0;
  uint64_t prop_conn_trigger_lookups = 0;
  uint64_t prop_conn_trigger_hits = 0;
  uint64_t prop_comb_trigger_lookups = 0;
  uint64_t prop_comb_trigger_hits = 0;
  uint64_t prop_enqueue_attempts = 0;
  uint64_t prop_enqueue_deduped = 0;

  // Propagation entry-point breakdown.
  uint64_t prop_calls_total = 0;
  uint64_t prop_calls_with_work = 0;
  uint64_t prop_calls_without_work = 0;
  uint64_t prop_pending_slots_total = 0;
};

// Per-process wakeup/activation counters. Collected when kDetailedStats is
// enabled. Allows post-simulation analysis of which processes are hot,
// what wakes them, and what kind of work each activation does.
struct ProcessWakeStats {
  // Wake attempts by cause (includes deduped).
  uint64_t wake_edge = 0;
  uint64_t wake_change = 0;
  uint64_t wake_container = 0;
  uint64_t wake_delay = 0;
  uint64_t wake_initial = 0;
  uint64_t wake_other = 0;
  // Deduped wake attempts (process already enqueued).
  uint64_t wake_deduped = 0;
  // Actual activations (process body executed).
  uint64_t runs = 0;
  // Total distinct slots dirtied (direct MarkSlotDirty) across all runs.
  uint64_t total_slots_dirtied = 0;
  // Activations with no direct dirty marks. These are typically always_ff
  // processes that write only via NBA (nonblocking assignment). NOT a
  // wasted-wakeup indicator: always_ff processes legitimately schedule
  // NBAs without calling MarkSlotDirty during their body.
  uint64_t nba_only_runs = 0;
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
// Concrete InstanceTraceResolver keyed by RuntimeInstance::instance_id.
// Built once from the instance list; validates uniqueness and null-free.
// Lookup is O(1) via dense table indexed by instance_id.
class InstanceIdTraceResolver final : public trace::InstanceTraceResolver {
 public:
  void Build(std::span<RuntimeInstance* const> instances);

  [[nodiscard]] auto FindInstance(InstanceId instance_id) const
      -> const RuntimeInstance* override {
    if (instance_id.value >= lookup_.size()) return nullptr;
    return lookup_[instance_id.value];
  }

  // Mutable lookup for engine-internal use.
  [[nodiscard]] auto FindInstanceMut(InstanceId instance_id) const
      -> RuntimeInstance* {
    if (instance_id.value >= lookup_mut_.size()) return nullptr;
    return lookup_mut_[instance_id.value];
  }

 private:
  std::vector<const RuntimeInstance*> lookup_;
  std::vector<RuntimeInstance*> lookup_mut_;
};

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
    if (detailed_stats_enabled_) {
      per_process_stats_.resize(num_processes);
    }
    decision_owner_tables_.resize(num_processes);
    decision_owner_states_.resize(num_processes);
    decision_owner_pending_flags_.resize(num_processes, 0);
    deferred_assertion_states_.resize(num_processes);
    deferred_pending_flags_.resize(num_processes, 0);
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
  // R5: Subscribe with typed signal identity. Dispatches once at the top
  // boundary, then routes to domain-specific helpers. No `is_local` below.
  auto Subscribe(
      ProcessHandle handle, ResumePoint resume, SignalRef signal,
      common::EdgeKind edge, bool initially_active = true) -> uint32_t;
  auto Subscribe(
      ProcessHandle handle, ResumePoint resume, SignalRef signal,
      common::EdgeKind edge, uint32_t byte_offset, uint32_t byte_size,
      uint8_t bit_index, bool initially_active = true) -> uint32_t;
  auto SubscribeContainerElement(
      ProcessHandle handle, ResumePoint resume, SignalRef signal,
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
      std::span<const DepSignalRecord> dep_records);

  // Create rebind subscriptions for a late-bound edge trigger. For each dep
  // slot, an AnyChange rebind node is created. When any dep changes, the plan
  // is re-evaluated and the edge target's observation position is updated.
  // edge_target_id identifies the target in edge_target_table_.
  // target_kind must be kEdge or kContainer (kChange not supported).
  //
  // R5: Domain-specific. Global deps install into signal_subs_, local deps
  // into instance observability. dep_slots values are global slot_ids or
  // local signal_ids matching the target's domain.
  void SubscribeRebind(
      ProcessHandle handle, uint32_t edge_target_id, SignalRef target_signal,
      SubKind target_kind, uint32_t target_index, uint8_t target_edge_group,
      EdgeBucket target_edge_bucket, std::span<const IndexPlanOp> plan,
      BitTargetMapping mapping, std::span<const SignalRef> dep_signals);

  // Schedule process to resume in the next delta cycle (same time).
  // Used for kRepeat terminator.
  void ScheduleNextDelta(ProcessHandle handle, ResumePoint resume);

  // Enqueue a non-blocking assignment for later commit in the NBA region.
  // mask_ptr == nullptr: full overwrite (direct compare/copy).
  // mask_ptr != nullptr: masked merge (per-byte mask).
  void ScheduleNba(
      void* write_ptr, const void* notify_base_ptr, const void* value_ptr,
      const void* mask_ptr, uint32_t byte_size, NbaNotifySignal notify_signal);

  // Schedule a canonical two-plane packed narrow NBA write as one semantic
  // record. Writes region_byte_size bytes to write_ptr (value plane) and to
  // write_ptr + second_region_offset (unknown plane).
  void ScheduleNbaCanonicalPacked(
      void* write_ptr, const void* notify_base_ptr, const void* value_ptr,
      const void* unk_ptr, uint32_t region_byte_size,
      uint32_t second_region_offset, NbaNotifySignal notify_signal);

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
    if (!finished_) {
      finished_ = true;
      end_reason_ = SimulationEndReason::kFinish;
    }
  }

  // Runtime counters accumulated during simulation.
  [[nodiscard]] auto GetStats() const -> const RuntimeStats& {
    return stats_;
  }

  // Print runtime stats (propagation + scheduler) to sink.
  void DumpRuntimeStats(FILE* sink) const;

  // Structured scheduler introspection.
  [[nodiscard]] auto TakeSchedulerSnapshot() const -> SchedulerSnapshot;
  static void RenderSchedulerSnapshot(
      FILE* sink, const SchedulerSnapshot& snapshot);

  [[nodiscard]] auto EndReason() const -> SimulationEndReason {
    return end_reason_;
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
    // Flat path (no bundles): all slots are design-global.
    global_slot_count_ = registry.Size();
    slot_meta_registry_ = std::move(registry);
  }

  // Set once at simulation init. All process states share the same DesignState
  // pointer (codegen invariant: one DesignState per LyraRunSimulation call).
  void SetDesignStateBase(void* base) {
    design_state_base_ = base;
  }

  // Set the instance list for slot storage resolution and observability.
  // Engine copies the pointers and owns the canonical mutable list.
  // All callers read through instances_.
  void SetInstances(std::span<const RuntimeInstance* const> instances);

  // R5: Instance trace resolver for trace sinks. Resolves instance_id
  // to RuntimeInstance through explicit validated lookup, not positional
  // indexing. Must be called after SetInstances.
  [[nodiscard]] auto GetInstanceTraceResolver() const
      -> const trace::InstanceTraceResolver& {
    return instance_trace_resolver_;
  }

  // R5: Typed instance lookup by stable InstanceId.
  // These are the ONLY approved way to go from InstanceId to RuntimeInstance
  // in semantic/runtime code. Never index instances_ by InstanceId.value.
  [[nodiscard]] auto FindInstance(InstanceId id) const
      -> const RuntimeInstance* {
    return instance_trace_resolver_.FindInstance(id);
  }
  [[nodiscard]] auto FindInstanceMut(InstanceId id) -> RuntimeInstance* {
    return instance_trace_resolver_.FindInstanceMut(id);
  }
  [[nodiscard]] auto GetInstance(InstanceId id) const
      -> const RuntimeInstance& {
    const auto* inst = instance_trace_resolver_.FindInstance(id);
    if (inst == nullptr) {
      throw common::InternalError(
          "Engine::GetInstance",
          std::format("no instance for instance_id {}", id));
    }
    return *inst;
  }
  [[nodiscard]] auto GetInstanceMut(InstanceId id) -> RuntimeInstance& {
    auto* inst = instance_trace_resolver_.FindInstanceMut(id);
    if (inst == nullptr) {
      throw common::InternalError(
          "Engine::GetInstanceMut",
          std::format("no instance for instance_id {}", id));
    }
    return *inst;
  }

  // Validate that all kInstanceOwned slot-meta entries reference valid
  // instances. Must be called after both slot meta and instance list are
  // installed. Throws InternalError on any mismatch.
  void ValidateInstanceOwnedSlotMeta() const;

  // R4: Initialize all module-instance runtime registries from per-instance
  // bundles and body templates. Replaces the flat-table init path for
  // module instances (InitSlotMeta/InitProcessTriggerRegistry/InitCombKernels
  // for module-instance data). Connection and design-global init remains
  // separate.
  //
  // This method:
  //   1. Computes per-instance flat coordinate bases from bundle slot counts.
  //   2. Builds the slot meta registry from design-global words + bundles.
  //   3. Builds module-instance trigger descriptors from body templates.
  //   4. Builds module-instance comb kernels from body templates.
  //   5. Builds module-instance process meta from body templates.
  //   6. Sizes coordination arrays (update_set, signal_subs).
  //
  // Must be called after SetInstances. Must be called before connection
  // trigger/comb init (which merge into the registries built here).
  void InitModuleInstancesFromBundles(
      std::span<const InstanceMetadataBundle> bundles,
      std::span<const uint32_t> design_global_slot_meta_words, void** states);

  // Resolve connection destination byte address via typed target.
  // Global targets resolve via design_state_base_.
  // Local targets resolve via instance resolver + local slot meta.
  [[nodiscard]] auto ResolveConnectionDstMut(const ConnectionTarget& dst)
      -> uint8_t*;

  // Resolve the storage byte address for a slot.
  // For kDesignGlobal slots: returns design_state_base_ + design_base_off.
  // For kInstanceOwned slots: returns instance->storage.inline_base +
  // instance_rel_off.
  [[nodiscard]] auto ResolveSlotBytes(uint32_t slot_id) const -> const uint8_t*;
  [[nodiscard]] auto ResolveSlotBytesMut(uint32_t slot_id) -> uint8_t*;

  // R5: Global slot resolution by GlobalSignalId.
  [[nodiscard]] auto ResolveGlobalSlotBase(GlobalSignalId signal) const
      -> const uint8_t*;
  [[nodiscard]] auto ResolveGlobalSlotBaseMut(GlobalSignalId signal)
      -> uint8_t*;

  // Release all string-typed slot handles. Called at shutdown, before
  // engine destruction. Uses engine-owned slot meta and slot resolution.
  void ReleaseStringSlots();

  // Route a MutationEvent to the UpdateSet (design slots only; heap NYI).
  void OnMutation(const common::MutationEvent& event);

  // Internal flat-slot coordination methods. These accept raw uint32_t
  // slot ids and are used by engine internals (connections, comb fixpoint,
  // typed API wrappers) and a few legacy runtime utility paths (dyn_array,
  // io). New code should use the typed API (ObjectSignalRef/GlobalSignalId)
  // instead.

  // First-dirty slow path.
  // Precondition: slot_id must be a canonical storage owner.
  // Alias canonicalization is compile-time (EmitMutationTargetSignalCoord)
  // and descriptor-time (RebuildCanonicalConnections). Runtime dirty
  // paths assume canonical slot ids. Trace flush is the backstop.
  void MarkSlotDirtyFirst(uint32_t slot_id) {
    if (detailed_stats_enabled_) ++stats_.detailed.dirty_mark_calls;
    update_set_.MarkSlotDirty(slot_id);
    if (activation_ctx_.active) {
      NoteActivationDirty(slot_id);
    }
  }

  // Mark slot dirty for scheduler wakeup and deferred trace snapshot.
  void MarkSlotDirty(uint32_t slot_id) {
    if (detailed_stats_enabled_) {
      ++stats_.detailed.dirty_mark_calls;
    }
    bool first_dirty = update_set_.MarkSlotDirty(slot_id);
    if (detailed_stats_enabled_) {
      if (first_dirty) {
        ++stats_.detailed.dirty_mark_first_touch;
      } else {
        ++stats_.detailed.dirty_mark_fast_path;
      }
    }
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

  // Typed signal coordination API (R3).
  // Accepts object-local or global signal coordinates. Internally lowers to
  // flat slot id for current coordination storage. The flat conversion is
  // engine-internal; callers use typed coordinates only.
  void MarkDirty(ObjectSignalRef signal);
  void MarkDirtyRange(
      ObjectSignalRef signal, uint32_t byte_off, uint32_t byte_size);
  void MarkDirty(GlobalSignalId signal);
  void MarkDirtyRange(
      GlobalSignalId signal, uint32_t byte_off, uint32_t byte_size);
  void ScheduleNba(
      ObjectSignalRef notify_signal, void* write_ptr,
      const void* notify_base_ptr, const void* value_ptr, const void* mask_ptr,
      uint32_t byte_size);
  void ScheduleNba(
      GlobalSignalId notify_signal, void* write_ptr,
      const void* notify_base_ptr, const void* value_ptr, const void* mask_ptr,
      uint32_t byte_size);
  void ScheduleNbaCanonicalPacked(
      ObjectSignalRef notify_signal, void* write_ptr,
      const void* notify_base_ptr, const void* value_ptr, const void* unk_ptr,
      uint32_t region_byte_size, uint32_t second_region_offset);
  void ScheduleNbaCanonicalPacked(
      GlobalSignalId notify_signal, void* write_ptr,
      const void* notify_base_ptr, const void* value_ptr, const void* unk_ptr,
      uint32_t region_byte_size, uint32_t second_region_offset);
  [[nodiscard]] auto IsTraceObserved(ObjectSignalRef signal) const -> bool;
  [[nodiscard]] auto IsTraceObserved(GlobalSignalId signal) const -> bool;

  [[nodiscard]] auto GetSlotMetaRegistry() const -> const SlotMetaRegistry& {
    return slot_meta_registry_;
  }

  // Get hierarchical path for an instance (%m support).
  // Throws InternalError for invalid instance_id (compiler/runtime bug).
  [[nodiscard]] auto GetInstancePath(InstanceId instance_id) const
      -> std::string_view {
    if (instance_id.value >= instance_paths_.size()) {
      throw common::InternalError(
          "Engine::GetInstancePath",
          std::format(
              "invalid instance_id {} (have {} instances)", instance_id,
              instance_paths_.size()));
    }
    return instance_paths_[instance_id.value];
  }

  // Build the DPI scope registry from instances_ and instance_paths_.
  // Must be called after SetInstances and before simulation start.
  void BuildDpiScopeRegistry();

  // Validate an svScope handle against the authoritative registry.
  // Returns nullptr for null scope. Throws InternalError for non-null
  // handles that are not registered (prevents dangling pointer use).
  [[nodiscard]] auto ValidateScopeHandle(svScope scope) const
      -> const RuntimeInstance*;

  // Non-throwing scope handle validity check.
  // Returns true if inst is registered in the DPI scope registry.
  [[nodiscard]] auto IsScopeHandleValid(const RuntimeInstance* inst) const
      -> bool;

  // Resolve a hierarchical path to an instance scope handle.
  // Returns nullptr if path is not found.
  [[nodiscard]] auto ResolveScopeByPath(std::string_view path) const
      -> const RuntimeInstance*;

  // Get the canonical hierarchical path for a scope handle.
  // Returns nullptr for null inst. Throws InternalError for unregistered inst.
  [[nodiscard]] auto GetScopePath(const RuntimeInstance* inst) const -> const
      char*;

  // Per-scope user-data storage (svPutUserData/svGetUserData).
  // Returns 0 on success, -1 on error (null scope or null key).
  auto PutScopeUserData(const RuntimeInstance* inst, void* key, void* data)
      -> int;

  // Returns nullptr on error (null scope, null key, or key not found).
  [[nodiscard]] auto GetScopeUserData(
      const RuntimeInstance* inst, void* key) const -> void*;

  // D6d: Per-instance time metadata, populated from BodyRealizationDesc.
  void InitInstanceTimeMetadata(
      std::span<const InstanceMetadataBundle> bundles);

  // D6d: Simulation-level time semantics for null-scope queries.
  struct SimulationTimeSemantics {
    int8_t unit_power;
    int8_t precision_power;
  };
  [[nodiscard]] auto GetSimulationTimeSemantics() const
      -> SimulationTimeSemantics;

  // D6d: Scope-aware time unit/precision queries.
  // null inst -> simulation-level semantics.
  // non-null inst -> per-instance metadata from body descriptor.
  [[nodiscard]] auto GetScopeTimeUnitPower(const RuntimeInstance* inst) const
      -> int32_t;
  [[nodiscard]] auto GetScopeTimePrecisionPower(
      const RuntimeInstance* inst) const -> int32_t;

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

  // Initialize comb kernels from word table. Resolves shared body pointers
  // from frame headers (constructor-owned binding). Builds trigger map.
  // num_connection: connection process count (partition boundary).
  // states: full process state array, indexed by proc_idx from word table.
  void InitCombKernels(
      std::span<const uint32_t> words, uint32_t num_connection, void** states);

  // Mark all comb kernel trigger slots dirty to ensure initial evaluation.
  void SeedCombKernelDirtyMarks();

  // Parse process trigger word table and build constructor-time trigger
  // groups in one call. Must be called after InitSlotMeta and SetInstances.
  // Body-local trigger entries (kFlagBodyLocal) are relocated to dense
  // coordination coordinates using the owning instance's base.
  void InitProcessTriggerRegistry(
      std::span<const uint32_t> words, uint32_t num_connection, void** states);

  // Flush signal updates + evaluate triggered connections/comb kernels until
  // convergence. Also used for initial value propagation before Run().
  void FlushAndPropagateConnections();

  // Frontier promotion helpers for FlushAndPropagateConnections.
  // Retire old current frontier (clear pending), promote next frontier
  // (swap next->pending, clear dedup), swap frontier vectors.
  void PromoteLocalFrontier();
  void PromoteGlobalFrontier();

  // One-time init for process metadata registry.
  // R4: When pending module meta words exist (from bundle init), merges
  // connection-only registry with module process meta into one combined
  // registry. The merged registry covers all processes.
  void InitProcessMeta(ProcessMetaRegistry connection_registry);

  // One-time init for back-edge site metadata registry.
  void InitBackEdgeSiteMeta(BackEdgeSiteRegistry registry) {
    back_edge_site_meta_ = std::move(registry);
  }

  // One-time init for immediate cover hit-count array.
  void InitImmediateCoverSites(uint32_t num_sites) {
    immediate_cover_counts_.assign(num_sites, 0);
  }

  // Record a hit for an immediate cover site. site_index is compiler-generated
  // and runtime-sized from compiler metadata; mismatch is a compiler bug.
  void RecordImmediateCoverHit(uint32_t site_index) {
    if (site_index >= immediate_cover_counts_.size()) {
      throw common::InternalError(
          "Engine::RecordImmediateCoverHit",
          "immediate cover site index out of range");
    }
    immediate_cover_counts_[site_index]++;
  }

  [[nodiscard]] auto NumImmediateCoverSites() const -> uint32_t {
    return static_cast<uint32_t>(immediate_cover_counts_.size());
  }

  // Query hit count for an immediate cover site. No consumers yet;
  // will be used by future reporting infrastructure and test harnesses.
  // Strict bounds check: out-of-range is a compiler/runtime metadata
  // mismatch.
  [[nodiscard]] auto GetImmediateCoverHitCount(uint32_t site_index) const
      -> uint64_t {
    if (site_index >= immediate_cover_counts_.size()) {
      throw common::InternalError(
          "Engine::GetImmediateCoverHitCount",
          "immediate cover site index out of range");
    }
    return immediate_cover_counts_[site_index];
  }

  // One-time init for wait-site metadata registry.
  void InitWaitSiteMeta(WaitSiteRegistry registry) {
    wait_site_meta_ = std::move(registry);
  }

  // Add a waiter to a named event object. Called by activation
  // post-processing when a process suspends with kWaitEvent.
  // key: (instance_id, local_event_id) uniquely identifying the runtime
  // event object within this design.
  void AddEventWaiter(EventObjectKey key, EventWaiter waiter) {
    event_registry_.AddWaiter(key, waiter);
  }

  // Consume all waiters for a named event object and enqueue them for
  // wakeup. Called by LyraTriggerEvent ABI function.
  // key: (instance_id, local_event_id) uniquely identifying the runtime
  // event object within this design.
  void TriggerEvent(EventObjectKey key) {
    auto waiters = event_registry_.ConsumeWaiters(key);
    for (const auto& w : waiters) {
      EnqueueProcessWakeup(
          w.process_id, w.instance_id, w.resume_block, key.local_event_id,
          WakeCause::kEvent);
    }
  }

  // One-time init for global trace signal metadata registry. Must be called
  // exactly once. Engine owns both the registry and the global selection mask.
  // The non-owning pointer passed to TraceManager remains valid for Engine's
  // lifetime because trace_signal_meta_ is never moved after this call.
  //
  // R5: This registry is global-only. Instance-owned trace metadata lives
  // in BodyObservableLayout::trace_meta, accessed via RuntimeInstance.
  void InitTraceSignalMeta(TraceSignalMetaRegistry registry) {
    if (trace_signal_meta_.IsPopulated()) {
      throw common::InternalError(
          "Engine::InitTraceSignalMeta",
          "trace signal metadata already initialized");
    }

    // R5: Global trace meta is indexed by GlobalSignalId. The count must
    // match the global slot count (constructor builds both in lockstep).
    auto trace_count = static_cast<uint32_t>(registry.Count());
    if (global_slot_count_ > 0 && trace_count != global_slot_count_) {
      throw common::InternalError(
          "Engine::InitTraceSignalMeta",
          std::format(
              "global trace meta count {} does not match global slot "
              "count {}",
              trace_count, global_slot_count_));
    }

    trace_signal_meta_ = std::move(registry);

    // Rebuild alias groups with cross-registry validation when slot
    // metadata is available. This validates total_bytes compatibility
    // between alias and owner at the alias-group boundary.
    const SlotMetaRegistry* slot_reg =
        slot_meta_registry_.IsPopulated() ? &slot_meta_registry_ : nullptr;
    trace_signal_meta_.BuildAliasGroups(slot_reg);

    trace_manager_.SetSignalMeta(&trace_signal_meta_);
  }

  [[nodiscard]] auto GetTraceSignalMetaRegistry() const
      -> const TraceSignalMetaRegistry& {
    return trace_signal_meta_;
  }

  // R5: Initialize trace selection to cover all flat slot_ids.
  // Must be called after InitSlotMeta / InitModuleInstancesFromBundles
  // and optionally after InitTraceSignalMeta. The selection must cover
  // instance-owned flat slot_ids because the compiled code's trace guard
  // uses flat slot_id to check selection.
  void InitTraceSelection() {
    if (trace_selection_.IsConfigured()) return;
    uint32_t count = slot_meta_registry_.IsPopulated()
                         ? slot_meta_registry_.Size()
                         : static_cast<uint32_t>(trace_signal_meta_.Count());
    if (count > 0) {
      trace_selection_.Init(count);
    }
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

  // Format decision owner identity for diagnostics.
  // First cut: delegates to FormatProcess (1:1 owner-to-process mapping).
  [[nodiscard]] auto FormatDecisionOwner(DecisionOwnerId owner_id) const
      -> std::string;

  // Async-signal-safe dump of scheduler status to fd.
  // Prints: phase, sim_time, activation_seq, current/last process.
  void DumpSchedulerStatusAsyncSignalSafe(int fd) const;

  // Get current running process ID.
  [[nodiscard]] auto CurrentRunningProcessId() const -> uint32_t {
    return current_running_process_.load(std::memory_order_acquire);
  }

  // Handle a trap raised by generated code (loop budget exceeded, etc.).
  void HandleTrap(uint32_t process_id, const TrapPayload& payload);

  // Record a decision observation for an explicit owner.
  // Called from LyraRecordDecisionObservation (extern "C" ABI boundary).
  void RecordDecisionObservation(
      DecisionOwnerId owner_id, DecisionId decision_id, MatchClass match_class,
      DecisionSelectedKind selected_kind, DecisionArmIndex selected_arm);

  // Scheduler phase for signal-safe status dump.
  enum class Phase : uint32_t {
    kIdle,
    kAdvanceTime,
    kRunProcess,
    kFlushUpdates,
    kCommitNba,
    kSettleComplete,
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

  // Decision settle-complete validation and diagnostics.
  void RunSettleCompleteChecks();

 public:
  // Deferred assertion runtime API (A2).
  void InitDeferredAssertionSites(
      const struct LyraDeferredAssertionSiteMeta* sites, uint32_t count);
  void EnqueueDeferredAssertion(
      uint32_t process_id, uint32_t instance_id, uint32_t site_id,
      uint8_t disposition, const void* payload_ptr, uint32_t payload_size,
      const DeferredAssertionRefBindingAbi* ref_ptr, uint32_t ref_count);
  void FlushDeferredAssertionsForProcess(ProcessId pid);
  void MatureAndExecuteObservedDeferredAssertions();

 private:
  void ValidateDecisionOwnerChecks(DecisionOwnerId owner_id);
  [[nodiscard]] auto BuildDecisionViolationMessage(
      DecisionOwnerId owner_id, const DecisionMetaEntry& meta,
      DecisionViolation violation) const -> std::string;
  [[nodiscard]] auto BuildDecisionViolationReport(
      DecisionOwnerId owner_id, const DecisionMetaEntry& meta,
      DecisionViolation violation) const -> ReportRequest;

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
  void RemoveChangeSub(const SubRef& ref);
  void RemoveRebindWatcherSub(const SubRef& ref);
  void RemoveContainerSub(const SubRef& ref);

  // R5: Domain-split subscribe helpers. Top boundary dispatches once
  // via std::visit on SignalRef, then routes to one of these.
  auto SubscribeGlobalEdge(
      ProcessHandle handle, ResumePoint resume, GlobalSignalId signal,
      common::EdgeKind edge, uint32_t byte_offset, uint32_t byte_size,
      uint8_t bit_index, bool initially_active) -> uint32_t;
  auto SubscribeLocalEdge(
      ProcessHandle handle, ResumePoint resume, LocalSignalRef signal,
      common::EdgeKind edge, uint32_t byte_offset, uint32_t byte_size,
      uint8_t bit_index, bool initially_active) -> uint32_t;
  auto SubscribeGlobalChange(
      ProcessHandle handle, ResumePoint resume, GlobalSignalId signal,
      uint32_t byte_offset, uint32_t byte_size, bool initially_active)
      -> uint32_t;
  auto SubscribeLocalChange(
      ProcessHandle handle, ResumePoint resume, LocalSignalRef signal,
      uint32_t byte_offset, uint32_t byte_size, bool initially_active)
      -> uint32_t;
  auto SubscribeGlobalContainerElement(
      ProcessHandle handle, ResumePoint resume, GlobalSignalId signal,
      common::EdgeKind edge, int64_t sv_index, uint32_t elem_stride,
      bool initially_active) -> uint32_t;
  auto SubscribeLocalContainerElement(
      ProcessHandle handle, ResumePoint resume, LocalSignalRef signal,
      common::EdgeKind edge, int64_t sv_index, uint32_t elem_stride,
      bool initially_active) -> uint32_t;
  void ValidateRebindDepSignals(std::span<const SignalRef> dep_signals) const;
  void InstallRebindDepWatchers(
      ProcessHandle handle, uint32_t edge_target_id,
      std::span<const SignalRef> dep_signals);
  void SubscribeGlobalRebind(
      ProcessHandle handle, uint32_t edge_target_id,
      GlobalSignalId target_signal, SubKind target_kind, uint32_t target_index,
      uint8_t target_edge_group, EdgeBucket target_edge_bucket,
      std::span<const IndexPlanOp> plan, BitTargetMapping mapping,
      std::span<const SignalRef> dep_signals);
  void SubscribeLocalRebind(
      ProcessHandle handle, uint32_t edge_target_id,
      LocalSignalRef target_signal, SubKind target_kind, uint32_t target_index,
      uint8_t target_edge_group, EdgeBucket target_edge_bucket,
      std::span<const IndexPlanOp> plan, BitTargetMapping mapping,
      std::span<const SignalRef> dep_signals);

  // Persistent wait-site installation
  void InstallWaitSite(
      ProcessHandle handle, SuspendRecord* suspend,
      const CompiledWaitSite& descriptor);
  void RefreshInstalledSnapshots(ProcessHandle handle);

  // Late-bound rebinding: re-read index value, recompute edge target.
  void RebindSubscription(uint32_t edge_target_id);

  // Per-dirty-slot flush: edge, change, and container dispatch.
  // Called after the global rebind phase completes.
  void FlushDirtySlotPostRebind(
      uint32_t slot_id, SlotSubscriptions& slot,
      std::span<const uint8_t> slot_storage);

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

  // R5: Domain-split flush entrypoints.
  // Global flush iterates global dirty slots. Local flush iterates one
  // instance's local dirty signals. Currently these are stub entrypoints
  // that route to the existing flat subscription infrastructure for
  // signals that have subscriptions. Later cuts make them the primary
  // flush path.
  void FlushGlobalSignalUpdates();
  void FlushLocalSignalUpdates(RuntimeInstance& inst);

  // R5: Resolve SlotSubscriptions for a sub reference (local or global).
  auto ResolveSubSlot(const SubRef& ref) -> SlotSubscriptions&;
  auto ResolveSubSlot(const SubRef& ref) const -> const SlotSubscriptions&;
  auto ResolveSubSlot(uint32_t slot_id, bool is_local, InstanceId instance_id)
      -> SlotSubscriptions&;

  // Per-kind flush helpers. Callers resolve slot storage before calling.
  void FlushSlotRebindSubs(
      std::vector<RebindWatcherSub>& subs,
      std::span<const uint8_t> slot_storage);
  void FlushSlotEdgeGroups(
      uint32_t slot_id, std::vector<EdgeWatchGroup>& groups,
      std::span<const uint8_t> slot_storage,
      const common::RangeSet& dirty_ranges, RangeFilterMode mode);
  void UpdateEdgeColdSnapshots(EdgeWatchGroup& group, uint8_t current_byte);
  void FlushSlotChangeSubs(
      uint32_t slot_id, std::vector<ChangeSub>& subs,
      std::span<const uint8_t> slot_storage,
      const common::RangeSet& dirty_ranges, RangeFilterMode mode);
  void FlushSlotContainerSubs(
      uint32_t slot_id, std::vector<ContainerSub>& subs,
      std::span<const uint8_t> slot_storage);

  // Single container sub flush.
  void FlushContainerSub(
      uint32_t slot_id, ContainerSub& sub,
      std::span<const uint8_t> slot_storage);

  // R5: Native local dirty-slot flush (no synthetic SlotMeta).
  void FlushLocalDirtySlotPostRebind(
      RuntimeInstance& inst, LocalSignalId signal, SlotSubscriptions& slot);

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
    if (detailed_stats_enabled_) {
      ++stats_.detailed.wakeup_attempts;
      auto& ps = per_process_stats_[process_id];
      switch (cause) {
        case WakeCause::kEdge:
          ++ps.wake_edge;
          break;
        case WakeCause::kChange:
          ++ps.wake_change;
          break;
        case WakeCause::kContainer:
          ++ps.wake_container;
          break;
        case WakeCause::kDelay:
          ++ps.wake_delay;
          break;
        case WakeCause::kInitial:
          ++ps.wake_initial;
          break;
        default:
          ++ps.wake_other;
          break;
      }
    }
    if (process_states_[process_id].is_enqueued) {
      if (detailed_stats_enabled_) {
        ++stats_.detailed.wakeup_deduped;
        ++per_process_stats_[process_id].wake_deduped;
      }
      return;
    }
    next_delta_queue_.push_back(
        {process_id, InstanceId{instance_id}, resume_block});
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

  // R5: Clear per-instance local update sets (delta or full).
  void ClearLocalUpdatesDelta();
  void ClearLocalUpdates();

  ProcessDispatch process_dispatch_;
  uint32_t num_processes_ = 0;
  std::vector<ProcessState> process_states_;
  SimTime current_time_ = 0;
  bool finished_ = false;
  SimulationEndReason end_reason_ = SimulationEndReason::kEmptyQueues;
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

  // R5: Number of design-global (non-instance-owned) slots. Set during
  // slot meta initialization, used to validate global trace meta count.
  uint32_t global_slot_count_ = 0;

  // Slot metadata registry (empty until populated by JIT codegen).
  SlotMetaRegistry slot_meta_registry_;

  // Trace event manager (disabled by default, zero overhead when off).
  trace::TraceManager trace_manager_;

  // Shared dirty tracking for scheduler wakeup and trace snapshots.
  // design_state_base_: base pointer for DesignState (set once at init).
  // update_set_: tracks dirty slots (per-delta for scheduler, per-time-slot
  // for trace).
  void* design_state_base_ = nullptr;
  // Canonical owned instance pointer list. Both mutable and const views
  // are derived from this single source in SetInstances().
  // C++ span<T*> does not convert to span<const T*>, so both views need
  // separate backing storage.
  std::vector<RuntimeInstance*> instance_ptrs_;
  std::span<RuntimeInstance* const> instances_;  // mutable view
  std::vector<const RuntimeInstance*> const_instance_ptrs_;
  std::span<const RuntimeInstance* const> const_instances_;  // read-only view
  InstanceIdTraceResolver instance_trace_resolver_;
  // Reverse lookup: instance_id.value -> index in instances_[].
  // Built by SetInstances(). Sentinel UINT32_MAX for unoccupied slots.
  // Dense table indexed by instance_id.value. Uniqueness is enforced at
  // SetInstances() time; sparsity is bounded to prevent pathological
  // allocation (max_id <= 4 * count + 1024).
  std::vector<uint32_t> instance_to_idx_;

  // Canonical accessor for the reverse lookup. Validates bounds and sentinel.
  // Internal to the engine -- not exposed as a public API.
  [[nodiscard]] auto GetInstanceIndex(InstanceId id) const -> uint32_t;
  [[nodiscard]] auto GetInstanceIndex(const RuntimeInstance& inst) const
      -> uint32_t;

  // Derived sparse indexes summarizing which instances have non-empty
  // LocalUpdateSet state. Authoritative truth is always the per-instance
  // LocalUpdateSet; these are acceleration indexes for avoiding
  // full-instance sweeps.
  //
  // Delta-dirty: instances with non-empty DeltaDirtySignals() since the
  // last ClearLocalUpdatesDelta(). Consumed by FlushSignalUpdates,
  // ClearLocalUpdatesDelta, ReconcilePostActivation, fixpoint seed.
  std::vector<uint32_t> delta_dirty_instances_;
  std::vector<uint8_t> in_delta_dirty_;
  // Timeslot-dirty: instances with non-empty DirtySignals() since the
  // last ClearLocalUpdates(). Superset of delta-dirty. Consumed by
  // FlushLocalDirtySlotsToTrace, ClearLocalUpdates.
  std::vector<uint32_t> timeslot_dirty_instances_;
  std::vector<uint8_t> in_timeslot_dirty_;

  // Record that an instance has become locally dirty. Called from the
  // canonical local mark-dirty helpers below. Maintains both sparse
  // indexes with O(1) dedup.
  void MarkInstanceDeltaDirty(uint32_t instance_idx) {
    if (in_delta_dirty_[instance_idx] == 0) {
      in_delta_dirty_[instance_idx] = 1;
      delta_dirty_instances_.push_back(instance_idx);
    }
    if (in_timeslot_dirty_[instance_idx] == 0) {
      in_timeslot_dirty_[instance_idx] = 1;
      timeslot_dirty_instances_.push_back(instance_idx);
    }
  }

  // Canonical local dirty-mark helpers. ALL local dirty marking must
  // go through these methods -- never call local_updates.MarkSlotDirty()
  // or MarkDirtyRange() directly from engine code.
  // Overloads accepting instance_idx skip the GetInstanceIndex lookup
  // when the caller already has the index.
  void MarkLocalSignalDirty(RuntimeInstance& inst, LocalSignalId lid);
  void MarkLocalSignalDirty(
      RuntimeInstance& inst, LocalSignalId lid, uint32_t instance_idx);
  void MarkLocalSignalDirtyRange(
      RuntimeInstance& inst, LocalSignalId lid, uint32_t byte_off,
      uint32_t byte_size);
  void MarkLocalSignalDirtyRange(
      RuntimeInstance& inst, LocalSignalId lid, uint32_t byte_off,
      uint32_t byte_size, uint32_t instance_idx);

  UpdateSet update_set_;

  // Connection batch: fast-path for kernelized connection processes.
  // Instead of scheduling connection processes through the full engine,
  // connections are evaluated inline during signal propagation.
  struct BatchedConnection {
    uint32_t src_slot_id;  // flat, for ResolveSlotBytes (read-only)
    uint32_t byte_size;
    ConnectionTarget dst;
  };
  // All batched connections (for initial evaluation).
  std::vector<BatchedConnection> all_connections_;
  // R5: Global connection trigger map (indexed by GlobalSignalId.value,
  // sized to global_slot_count_). Per-instance local trigger maps live
  // on RuntimeInstanceObservability.
  std::vector<TriggerRange> global_conn_trigger_map_;

  // Comb kernel batch: pure combinational processes evaluated inline.
  // Each kernel has a compiled function pointer and state pointer.
  struct CombKernel {
    SharedBodyFn body;
    void* frame;
    uint32_t process_index;
    uint32_t flags;
    // Index into instances_[] for the owning module instance.
    // Always valid -- comb kernels are always module processes with a bound
    // RuntimeInstance (enforced at init time).
    uint32_t instance_idx;
    static constexpr uint32_t kSelfEdge = 1U;
  };
  std::vector<CombKernel> comb_kernels_;

  // Canonical comb-kernel construction. Populates instance_idx from the
  // process frame header via GetInstanceIndex(). Requires instance_to_idx_
  // to be built (i.e., after SetInstances()).
  auto BuildCombKernel(uint32_t proc_idx, void* frame, uint32_t flags)
      -> CombKernel;

  // Structured trigger entries with byte-range observation.
  struct CombTriggerEntry {
    uint32_t kernel_idx;
    uint32_t byte_offset;
    uint32_t byte_size;  // 0 = full-slot
    // Runtime acceleration copy; source of truth is CombKernel::flags.
    bool has_self_edge;
  };
  std::vector<CombTriggerEntry> comb_trigger_backing_;
  // R5: Global comb trigger map (indexed by GlobalSignalId.value,
  // sized to global_slot_count_). Per-instance local trigger maps live
  // on RuntimeInstanceObservability.
  std::vector<TriggerRange> global_comb_trigger_map_;
  // List of global trigger slots for seeding dirty marks.
  std::vector<GlobalSignalId> global_comb_trigger_slots_;
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

  // Named event registry for event-based synchronization.
  EventRegistry event_registry_;

  // Per-site hit counts for immediate cover statements.
  std::vector<uint64_t> immediate_cover_counts_;

  // Trace signal metadata registry (empty until populated).
  TraceSignalMetaRegistry trace_signal_meta_;

  // Per-slot selection mask for producer-side trace filtering.
  // Initialized alongside trace_signal_meta_ in InitTraceSignalMeta().
  TraceSelectionRegistry trace_selection_;

  // R5: Process-to-instance mapping. Indexed by process_id, gives the
  // owning instance_id for module processes. 0 for connection/init processes.
  std::vector<InstanceId> process_instance_map_;

  // R5: Per-body observable layouts (one per distinct shared body /
  // specialization). Keyed by body_key from InstanceMetadataBundle.
  // Each RuntimeInstance's observability.layout points into this storage.
  // The deque provides pointer stability across inserts.
  std::deque<BodyObservableLayout> body_observable_layouts_;

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

  // Per-process wakeup/activation counters (opt-in, kDetailedStats).
  // Indexed by process_id. Empty when detailed stats are disabled.
  std::vector<ProcessWakeStats> per_process_stats_;

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
    uint32_t slot_id;
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
  // R5: Domain-split fixpoint workspace for FlushAndPropagateConnections.
  // Global pending uses GlobalSignalId with dense bitvec dedup.
  // Local pending is per-instance with LocalSignalId and per-instance dedup.
  struct LocalPendingSet {
    RuntimeInstance* instance = nullptr;
    std::vector<LocalSignalId> pending;
    std::vector<LocalSignalId> next;
    std::vector<uint8_t> seen;  // sized to local_signal_count
  };

  struct FixpointWorkspace {
    // Global frontier
    std::vector<GlobalSignalId> pending_globals;
    std::vector<GlobalSignalId> next_globals;
    std::vector<uint8_t> global_pending_seen;

    // Local domain -- backing storage indexed by instance position
    std::vector<LocalPendingSet> locals;

    // Sparse instance frontiers. Only instances in current_instances are
    // consumed each iteration; only instances in next_instances receive
    // new work. Convergence is current_instances.empty().
    std::vector<uint32_t> current_instances;
    std::vector<uint32_t> next_instances;
    std::vector<uint8_t> in_next;  // bitvec: 1 iff in next_instances

    // Per-iteration comb-touched tracking. Records which instances had
    // a comb kernel executed, so local comb-write collection scans only
    // those instances. Reset each iteration.
    std::vector<uint32_t> comb_touched;
    std::vector<uint8_t> comb_touched_seen;
    // Pre-comb delta size, indexed by instance_idx. Valid only for
    // entries in comb_touched. Recorded exactly once per instance per
    // iteration, before the first comb execution for that instance.
    std::vector<uint32_t> delta_pre;

    // Comb write capture (split by domain)
    std::vector<GlobalSignalId> comb_writes_global;
    struct LocalCombWrite {
      uint32_t instance_idx;
      LocalSignalId signal;
    };
    std::vector<LocalCombWrite> comb_writes_local;

    // Self-edge snapshot (global signals only)
    std::vector<uint32_t> global_snapshot_index;
    std::vector<uint8_t> snapshot_buf;
    std::vector<CombSnapshot> snapshots;
    std::vector<GlobalSignalId> snapshotted_slots;
  };
  FixpointWorkspace fp_work_;

  // Constructor-time process trigger registry. Owns parsed descriptors,
  // trigger groups, and flat-backed group membership.
  ProcessTriggerRegistry process_trigger_registry_;

  // R4: Pending module-instance trigger descriptors built from bundles.
  // Merged with connection triggers in InitProcessTriggerRegistry.
  std::vector<ProcessTriggerDescriptor> pending_module_trigger_descs_;

  // R4: Narrow pending data for module-instance process meta assembly.
  // Stored during InitModuleInstancesFromBundles, consumed in
  // InitProcessMeta to build the combined process meta registry.
  struct PendingModuleProcessMeta {
    const BodyDescriptorPackage* body_desc = nullptr;
    const char* instance_path = nullptr;
    uint32_t module_proc_base = 0;
  };
  std::vector<PendingModuleProcessMeta> pending_module_process_meta_;

  // DPI scope registry (D6b). Built once by BuildDpiScopeRegistry().
  // Authoritative membership set for scope handle validation.
  std::unordered_set<const RuntimeInstance*> valid_scopes_;
  // Reverse path -> instance lookup for svGetScopeFromName.
  std::unordered_map<std::string_view, const RuntimeInstance*> scope_path_map_;
  // Reverse instance -> canonical path for svGetNameFromScope.
  std::unordered_map<const RuntimeInstance*, const char*> scope_inst_path_map_;
  // Per-scope user-data storage for svPutUserData/svGetUserData.
  // Outer key: instance pointer. Inner key: user-provided void* key.
  mutable std::unordered_map<
      const RuntimeInstance*, std::unordered_map<void*, void*>>
      scope_user_data_;

  // D6d: Per-instance time metadata, indexed by InstanceId::value.
  // Populated from BodyRealizationDesc during InitInstanceTimeMetadata.
  struct ScopeTimeMetadata {
    int8_t time_unit_power = 0;
    int8_t time_precision_power = 0;
    bool initialized = false;
  };
  std::vector<ScopeTimeMetadata> instance_time_metadata_;

  // Immutable per-owner decision metadata tables. Indexed by owner_id.
  // Populated during InitModuleInstancesFromBundles from body descriptor data.
  // Points into emitted LLVM globals; valid for program lifetime.
  std::vector<DecisionOwnerTable> decision_owner_tables_;
  // Per-owner mutable decision observation state. Indexed by owner_id.
  // Sized from the immutable table count during registration.
  std::vector<DecisionOwnerState> decision_owner_states_;
  // Dedup flag for pending_decision_owners_ (indexed by owner_id).
  std::vector<uint8_t> decision_owner_pending_flags_;
  // Owner IDs with pending decision checks this timeslot.
  std::vector<DecisionOwnerId> pending_decision_owners_;
  // Monotonic timeslot epoch for observation staleness detection.
  TimeslotEpoch current_timeslot_epoch_;
  // Per-(owner, site, violation) diagnostic counter for rate limiting.
  std::unordered_map<DecisionDiagKey, uint32_t, DecisionDiagKeyHash>
      decision_diag_counts_;

  // Deferred immediate assertion state (A2).
  // Borrowed pointer to registered site metadata (valid for program lifetime).
  const struct LyraDeferredAssertionSiteMeta* deferred_assertion_site_meta_ =
      nullptr;
  uint32_t num_deferred_assertion_sites_ = 0;

  struct DeferredAssertionRecord {
    uint32_t enqueue_generation = 0;
    uint32_t site_id = 0;
    uint8_t disposition = 0;
    uint32_t instance_id = 0;
    uint32_t payload_size = 0;
    SmallByteBuffer payload;
    // ref_bindings[i] corresponds 1:1 to the ith kLiveRef entry in the
    // site's realization actual_plan. Runtime preserves order and never
    // interprets entries.
    std::vector<DeferredAssertionRefBindingAbi> ref_bindings;
  };
  struct ProcessDeferredAssertionState {
    uint32_t flush_generation = 0;
    std::vector<DeferredAssertionRecord> pending;
  };
  std::vector<ProcessDeferredAssertionState> deferred_assertion_states_;
  std::vector<uint8_t> deferred_pending_flags_;
  std::vector<ProcessId> pending_deferred_processes_;
};

}  // namespace lyra::runtime
