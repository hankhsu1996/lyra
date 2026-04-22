#pragma once

#include <cstddef>
#include <cstdint>
#include <vector>

#include "lyra/common/deferred_assertion_abi.hpp"
#include "lyra/runtime/activation_trace.hpp"
#include "lyra/runtime/engine_subscriptions.hpp"
#include "lyra/runtime/process_frame.hpp"
#include "lyra/runtime/small_byte_buffer.hpp"

namespace lyra::runtime {

// Trace-only wakeup annotation. Populated at enqueue time only when
// activation tracing is enabled.
//
// Per-process storage is safe because each process has at most one pending
// wake source at a time:
//   - Delay/DelayZero/ScheduleNextDelta: called after ResetInstalledWait
//     clears all subscriptions, so flush cannot race with EnqueueProcessWakeup.
//   - EnqueueProcessWakeup: is_enqueued dedup prevents multiple writes
//     per delta.
//   - ScheduleInitial: one-time at startup, before any flush activity.
struct WakeTraceInfo {
  WakeCause cause = WakeCause::kDelay;
  uint32_t trigger_slot = kNoTriggerSlot;
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

struct DeferredAssertionRecord {
  uint32_t enqueue_generation = 0;
  uint32_t site_id = 0;
  uint8_t disposition = 0;
  RuntimeInstance* instance = nullptr;
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

// First-class runtime process object.
// Single owner of all per-process runtime state. Indexed by process_id
// in Engine::processes_.
struct RuntimeProcess {
  RuntimeInstance* instance = nullptr;

  // Compiled body function for this process. Written once during constructor
  // realization and never mutated afterward. Null for standalone connection
  // and init processes (which dispatch through LyraRunProcessSync instead).
  SharedBodyFn body = nullptr;

  bool is_enqueued = false;
  bool is_comb_kernel = false;

  size_t subscription_count = 0;
  std::vector<LocalSubRef> local_sub_refs;
  std::vector<GlobalSubRef> global_sub_refs;
  IndexPlanPool plan_pool;
  InstalledWaitState installed_wait;

  // Trace-only annotation set when an activation enters a queue. Mutated
  // only when activation tracing is enabled.
  WakeTraceInfo wake_trace{};

  // Wake/activation counters mutated only when kDetailedStats is enabled.
  ProcessWakeStats wake_stats{};

  // Pending deferred immediate assertion records produced by this process.
  ProcessDeferredAssertionState deferred_assertion_state{};

  // Dedup flag indicating this process is in pending_deferred_processes_.
  bool deferred_pending = false;

  // Back-pointer to this process's frame/state storage. Written once during
  // engine setup (Engine::RegisterFrameStates) and never mutated afterward.
  // The backing object begins with a SuspendRecord and is the single
  // runtime source of suspend state for this process.
  void* frame_state = nullptr;
};

}  // namespace lyra::runtime
