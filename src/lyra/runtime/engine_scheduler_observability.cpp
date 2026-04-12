#include <algorithm>
#include <array>
#include <cstdint>
#include <cstdio>
#include <numeric>
#include <span>
#include <string_view>
#include <unistd.h>
#include <vector>

#include <fmt/core.h>

#include "lyra/runtime/activation_trace.hpp"
#include "lyra/runtime/activation_trace_format.hpp"
#include "lyra/runtime/engine.hpp"
#include "lyra/runtime/engine_scheduler.hpp"
#include "lyra/runtime/engine_subscriptions.hpp"
#include "lyra/runtime/instance_observability.hpp"
#include "lyra/runtime/process_meta.hpp"
#include "lyra/runtime/runtime_instance.hpp"
#include "lyra/runtime/scheduler_snapshot.hpp"
#include "lyra/runtime/slot_meta.hpp"
#include "lyra/runtime/trace_signal_meta.hpp"

namespace lyra::runtime {

void Engine::DumpRuntimeStats(FILE* sink) const {
  const auto& c = stats_.core;

  // [core] line: always-on summary counters + static design shape metadata
  // (conn_full_slot, conn_narrow, comb_full_slot, comb_narrow).

  // Core sanity checks.
  if (c.activations_nba_only > c.total_activations) {
    fmt::print(
        sink,
        "[lyra][stats][warning] activations_nba_only > total_activations\n");
  }
  if (c.nba_elided > c.nba_entries) {
    fmt::print(sink, "[lyra][stats][warning] nba_elided > nba_entries\n");
  }
  uint64_t non_elided_nba = c.nba_entries - c.nba_elided;
  if (c.nba_changed > non_elided_nba) {
    fmt::print(
        sink, "[lyra][stats][warning] nba_changed > non-elided entries\n");
  }

  fmt::print(
      sink,
      "[lyra][stats][core]"
      " total_activations={} activations_nba_only={}"
      " propagation_calls={} propagation_iterations={}"
      " propagation_max_iterations={}"
      " nba_entries={} nba_elided={} nba_changed={}"
      " nba_generic_queue={} nba_deferred_local={}"
      " conn_full_slot={} conn_narrow={}"
      " comb_full_slot={} comb_narrow={}"
      " timeslots={} max_active_q={} max_next_delta_q={}"
      " max_delta_cycles={}\n",
      c.total_activations, c.activations_nba_only, c.propagation_calls,
      c.propagation_iterations, c.propagation_max_iterations, c.nba_entries,
      c.nba_elided, c.nba_changed, c.nba_generic_queue, c.nba_deferred_local,
      conn_full_slot_count_, conn_narrow_count_, comb_full_slot_count_,
      comb_narrow_count_, c.total_timeslots, c.max_active_queue,
      c.max_next_delta_queue, c.max_delta_cycles);

  // Trigger group stats (G13 metadata).
  const auto& reg = process_trigger_registry_;
  if (!reg.groups.empty() || !reg.descriptors.empty()) {
    // Count unique grouped and ungrouped process indices.
    std::vector<bool> seen(num_processes_, false);
    std::vector<bool> is_grouped(num_processes_, false);
    for (const auto& g : reg.groups) {
      for (uint32_t pi = g.process_start;
           pi < g.process_start + g.process_count; ++pi) {
        is_grouped[reg.group_process_backing[pi]] = true;
      }
    }
    uint32_t total_with_triggers = 0;
    uint32_t grouped_count = 0;
    for (const auto& d : reg.descriptors) {
      if (!seen[d.scheduled_process_index]) {
        seen[d.scheduled_process_index] = true;
        ++total_with_triggers;
        if (is_grouped[d.scheduled_process_index]) ++grouped_count;
      }
    }
    fmt::print(
        sink,
        "[lyra][stats][trigger_groups]"
        " trigger_groups={} grouped_processes={}"
        " ungrouped_processes={}\n",
        reg.groups.size(), grouped_count, total_with_triggers - grouped_count);
  }

  if (detailed_stats_enabled_) {
    const auto& d = stats_.detailed;

    // Detailed sanity checks.
    if (d.wakeup_deduped > d.wakeup_attempts) {
      fmt::print(
          sink, "[lyra][stats][warning] wakeup_deduped > wakeup_attempts\n");
    }
    if (d.edge_sub_wakeups > d.edge_sub_checks) {
      fmt::print(
          sink, "[lyra][stats][warning] edge_sub_wakeups > edge_sub_checks\n");
    }
    if (d.change_sub_wakeups > d.change_sub_checks) {
      fmt::print(
          sink,
          "[lyra][stats][warning] change_sub_wakeups > change_sub_checks\n");
    }

    fmt::print(
        sink,
        "[lyra][stats][detailed]"
        " dirty_mark_calls={}"
        " flush_dirty_slots={}"
        " conn_considered={} conn_memcmp={} conn_memcpy={}"
        " comb_considered={} comb_executed={} comb_skipped_range={}"
        " edge_sub_checks={} edge_sub_wakeups={}"
        " change_sub_checks={} change_sub_wakeups={}"
        " wakeup_attempts={} wakeup_deduped={}\n",
        d.dirty_mark_calls, d.flush_dirty_slots, d.conn_considered,
        d.conn_memcmp_executed, d.conn_memcpy_executed, d.comb_considered,
        d.comb_executed, d.comb_skipped_range, d.edge_sub_checks,
        d.edge_sub_wakeups, d.change_sub_checks, d.change_sub_wakeups,
        d.wakeup_attempts, d.wakeup_deduped);

    fmt::print(
        sink,
        "[lyra][stats][dirty_mark]"
        " calls={} fast_path={} first_touch={}\n",
        d.dirty_mark_calls, d.dirty_mark_fast_path, d.dirty_mark_first_touch);

    fmt::print(
        sink,
        "[lyra][stats][prop_worklist]"
        " pending_slots={}"
        " conn_lookups={} conn_hits={}"
        " comb_lookups={} comb_hits={}"
        " enqueue_attempts={} enqueue_deduped={}\n",
        d.prop_pending_slots, d.prop_conn_trigger_lookups,
        d.prop_conn_trigger_hits, d.prop_comb_trigger_lookups,
        d.prop_comb_trigger_hits, d.prop_enqueue_attempts,
        d.prop_enqueue_deduped);

    fmt::print(
        sink,
        "[lyra][stats][prop_entry]"
        " calls={} with_work={} without_work={}"
        " pending_total={}\n",
        d.prop_calls_total, d.prop_calls_with_work, d.prop_calls_without_work,
        d.prop_pending_slots_total);

    // Per-process wakeup/activation summary, sorted by total wake count
    // descending. Shows which processes are hot, what wakes them, and
    // what kind of work each activation does.
    //
    // Activation categories:
    //   direct_dirty: process called MarkSlotDirty (direct state change)
    //   nba_only:     no direct dirty marks; process likely writes via
    //                 NBA (nonblocking assignment). This is normal for
    //                 always_ff and is NOT a wasted-wakeup indicator.
    //   no_effect:    runs - direct_dirty - nba_only (should be 0;
    //                 nonzero means accounting bug)
    if (!per_process_stats_.empty()) {
      // Build sorted index by total wake attempts (descending).
      std::vector<uint32_t> order(per_process_stats_.size());
      std::ranges::iota(order, 0U);
      std::ranges::sort(order, [&](uint32_t a, uint32_t b) {
        auto total_a = per_process_stats_[a].wake_edge +
                       per_process_stats_[a].wake_change +
                       per_process_stats_[a].wake_container +
                       per_process_stats_[a].wake_delay +
                       per_process_stats_[a].wake_initial +
                       per_process_stats_[a].wake_other;
        auto total_b = per_process_stats_[b].wake_edge +
                       per_process_stats_[b].wake_change +
                       per_process_stats_[b].wake_container +
                       per_process_stats_[b].wake_delay +
                       per_process_stats_[b].wake_initial +
                       per_process_stats_[b].wake_other;
        return total_a > total_b;
      });

      fmt::print(
          sink, "[lyra][stats][per_process] {} processes\n",
          per_process_stats_.size());

      for (uint32_t pid : order) {
        const auto& ps = per_process_stats_[pid];
        uint64_t total_wakes = ps.wake_edge + ps.wake_change +
                               ps.wake_container + ps.wake_delay +
                               ps.wake_initial + ps.wake_other;
        if (total_wakes == 0 && ps.runs == 0) continue;

        std::string name;
        if (process_meta_.IsPopulated()) {
          name = process_meta_.Format(pid);
        } else {
          name = std::format("process_{}", pid);
        }

        uint64_t direct_dirty_runs =
            ps.runs > ps.nba_only_runs ? ps.runs - ps.nba_only_runs : 0;

        fmt::print(
            sink,
            "[lyra][stats][per_process] pid={} name={}"
            " wakes={} (edge={} change={} container={}"
            " delay={} initial={} other={})"
            " deduped={}"
            " runs={} direct_dirty={} nba_only={} dirtied={}\n",
            pid, name, total_wakes, ps.wake_edge, ps.wake_change,
            ps.wake_container, ps.wake_delay, ps.wake_initial, ps.wake_other,
            ps.wake_deduped, ps.runs, direct_dirty_runs, ps.nba_only_runs,
            ps.total_slots_dirtied);
      }
    }
  }

  std::fflush(sink);
}

namespace {

// Async-signal-safe: write a string literal to fd.
void SafeWrite(int fd, const char* str, size_t len) {
  static_cast<void>(write(fd, str, len));
}

// Async-signal-safe: write a uint64 as decimal to fd.
void SafeWriteU64(int fd, uint64_t val) {
  std::array<char, 20> storage{};
  auto buf = std::span(storage);
  int pos = 19;
  if (val == 0) {
    buf[pos] = '0';
    SafeWrite(fd, &buf[pos], 1);
    return;
  }
  while (val > 0) {
    buf[pos--] = static_cast<char>('0' + (val % 10));
    val /= 10;
  }
  SafeWrite(fd, &buf[pos + 1], static_cast<size_t>(19 - pos));
}

// Async-signal-safe: write process identity (name or <process N>).
void SafeWriteProcess(int fd, uint32_t pid, const ProcessMetaRegistry& meta) {
  if (pid == UINT32_MAX) {
    SafeWrite(fd, "none", 4);
    return;
  }
  if (meta.IsPopulated()) {
    meta.WriteAsyncSignalSafe(fd, pid);
  } else {
    SafeWrite(fd, "<process ", 9);
    SafeWriteU64(fd, pid);
    SafeWrite(fd, ">", 1);
  }
}

auto PhaseLabel(uint32_t phase) -> std::string_view {
  switch (static_cast<Engine::Phase>(phase)) {
    case Engine::Phase::kIdle:
      return "Idle";
    case Engine::Phase::kAdvanceTime:
      return "AdvanceTime";
    case Engine::Phase::kRunProcess:
      return "RunProcess";
    case Engine::Phase::kFlushUpdates:
      return "FlushUpdates";
    case Engine::Phase::kCommitNba:
      return "CommitNba";
    case Engine::Phase::kSettleComplete:
      return "SettleComplete";
    case Engine::Phase::kPostponed:
      return "Postponed";
  }
  return "Unknown";
}

}  // namespace

void Engine::DumpSchedulerStatusAsyncSignalSafe(int fd) const {
  // All reads are relaxed - this is a best-effort snapshot from a signal
  // handler. Values may be slightly stale but are always valid.
  uint32_t phase = phase_.load(std::memory_order_relaxed);
  uint64_t sim_time = current_time_;  // Not atomic, but read-only during dump
  uint64_t seq = activation_seq_.load(std::memory_order_relaxed);
  uint32_t current = current_running_process_.load(std::memory_order_relaxed);
  uint32_t last = last_process_id_.load(std::memory_order_relaxed);

  // Format: lyra: phase=X sim_time=T activations=N current=... last=...
  SafeWrite(fd, "lyra: phase=", 12);
  auto label = PhaseLabel(phase);
  SafeWrite(fd, label.data(), label.size());

  SafeWrite(fd, " sim_time=", 10);
  SafeWriteU64(fd, sim_time);

  SafeWrite(fd, " activations=", 13);
  SafeWriteU64(fd, seq);

  SafeWrite(fd, " current=", 9);
  SafeWriteProcess(fd, current, process_meta_);

  SafeWrite(fd, " last=", 6);
  SafeWriteProcess(fd, last, process_meta_);

  SafeWrite(fd, "\n", 1);
}

void Engine::TraceWake(const WakeupEntry& entry) {
  if (!activation_trace_.has_value()) return;
  const auto& trace = wake_trace_[entry.process_id];
  ActivationEvent ae{
      .time = current_time_,
      .delta = current_delta_,
      .process_id = entry.process_id,
      .trigger_slot = trace.trigger_slot,
      .resume_block = entry.resume_block,
      .kind = ActivationEventKind::kWake,
      .cause = trace.cause,
      .slots_dirtied = 0,
  };
  activation_trace_->Append(ae);
  fmt::print(stderr, "{}\n", FormatActivationEvent(ae, process_meta_));
}

void Engine::TraceRun(const WakeupEntry& entry) {
  if (!activation_trace_.has_value()) return;
  const auto& trace = wake_trace_[entry.process_id];
  ActivationEvent ae{
      .time = current_time_,
      .delta = current_delta_,
      .process_id = entry.process_id,
      .trigger_slot = trace.trigger_slot,
      .resume_block = entry.resume_block,
      .kind = ActivationEventKind::kRun,
      .cause = trace.cause,
      .slots_dirtied = activation_ctx_.dirty_count,
  };
  activation_trace_->Append(ae);
  fmt::print(stderr, "{}\n", FormatActivationEvent(ae, process_meta_));
}

namespace {

auto EndReasonLabel(SimulationEndReason reason) -> std::string_view {
  switch (reason) {
    case SimulationEndReason::kFinish:
      return "$finish called";
    case SimulationEndReason::kDeltaCycleLimit:
      return "delta cycle limit exceeded";
    case SimulationEndReason::kMaxTimeReached:
      return "max simulation time reached";
    case SimulationEndReason::kEmptyQueues:
      return "empty event queues (no $finish)";
    case SimulationEndReason::kTrap:
      return "process trap";
    case SimulationEndReason::kInterrupted:
      return "interrupted by signal";
  }
  return "unknown";
}

auto WaitKindLabel(ProcessWaitKind kind) -> std::string_view {
  switch (kind) {
    case ProcessWaitKind::kRunning:
      return "running";
    case ProcessWaitKind::kReady:
      return "ready";
    case ProcessWaitKind::kSuspendedDelay:
      return "suspended(delay)";
    case ProcessWaitKind::kSuspendedEdge:
      return "suspended(edge)";
    case ProcessWaitKind::kSuspendedChange:
      return "suspended(change)";
    case ProcessWaitKind::kSuspendedMulti:
      return "suspended(multi)";
    case ProcessWaitKind::kSuspendedRepeat:
      return "suspended(repeat)";
    case ProcessWaitKind::kSuspendedEvent:
      return "suspended(event)";
    case ProcessWaitKind::kSuspendedWait:
    case ProcessWaitKind::kSuspendedUnknown:
      return "suspended(unknown)";
    case ProcessWaitKind::kFinished:
      return "finished";
  }
  return "unknown";
}

auto EdgeBucketLabel(EdgeBucket bucket) -> std::string_view {
  return (bucket == EdgeBucket::kPosedge) ? "posedge" : "negedge";
}

auto SubKindLabel(SubKind kind) -> std::string_view {
  switch (kind) {
    case SubKind::kEdge:
      return "edge";
    case SubKind::kChange:
      return "change";
    case SubKind::kRebindWatcher:
      return "rebind_watcher";
    case SubKind::kContainer:
      return "container";
  }
  return "unknown";
}

// Refine kWait suspend tag into specific subscription kinds.
auto RefineWaitKind(
    const std::vector<LocalSubRef>& local_refs,
    const std::vector<GlobalSubRef>& global_refs) -> ProcessWaitKind {
  bool has_edge = false;
  bool has_change = false;
  bool has_container = false;
  auto classify = [&](SubKind kind) {
    switch (kind) {
      case SubKind::kEdge:
        has_edge = true;
        break;
      case SubKind::kChange:
        has_change = true;
        break;
      case SubKind::kContainer:
        has_container = true;
        break;
      case SubKind::kRebindWatcher:
        break;
    }
  };
  for (const auto& ref : local_refs) classify(ref.kind);
  for (const auto& ref : global_refs) classify(ref.kind);

  int count =
      (has_edge ? 1 : 0) + (has_change ? 1 : 0) + (has_container ? 1 : 0);
  if (count > 1) return ProcessWaitKind::kSuspendedMulti;
  if (has_edge) return ProcessWaitKind::kSuspendedEdge;
  if (has_change || has_container) return ProcessWaitKind::kSuspendedChange;
  return ProcessWaitKind::kSuspendedUnknown;
}

}  // namespace

auto Engine::TakeSchedulerSnapshot() const -> SchedulerSnapshot {
  SchedulerSnapshot snap;
  snap.time = current_time_;
  snap.delta = current_delta_;
  snap.end_reason = end_reason_;

  snap.active_queue_size = static_cast<uint32_t>(active_queue_.size());
  snap.inactive_queue_size = static_cast<uint32_t>(inactive_queue_.size());
  snap.next_delta_queue_size = static_cast<uint32_t>(next_delta_queue_.size());
  snap.delay_queue_entry_count = static_cast<uint32_t>(delay_queue_.size());

  // Build delay-queue lookup: pid -> target time.
  std::vector<SimTime> delay_target(num_processes_, 0);
  std::vector<bool> in_delay(num_processes_, false);
  for (const auto& [time, entries] : delay_queue_) {
    for (const auto& entry : entries) {
      if (entry.process_id < num_processes_) {
        in_delay[entry.process_id] = true;
        delay_target[entry.process_id] = time;
      }
    }
  }

  // Build ready-queue membership.
  std::vector<bool> is_ready(num_processes_, false);
  for (const auto& e : active_queue_) {
    if (e.process_id < num_processes_) is_ready[e.process_id] = true;
  }
  for (const auto& e : next_delta_queue_) {
    if (e.process_id < num_processes_) is_ready[e.process_id] = true;
  }

  uint32_t current_running =
      current_running_process_.load(std::memory_order_relaxed);

  // Design state for reading current bit values.
  auto design_state =
      (design_state_base_ != nullptr && slot_meta_registry_.IsPopulated())
          ? std::span(
                static_cast<const uint8_t*>(design_state_base_),
                slot_meta_registry_.MaxExtent())
          : std::span<const uint8_t>();

  // Signal name lookup helper.
  auto signal_name = [this](uint32_t slot_id) -> std::string {
    if (trace_signal_meta_.IsPopulated() &&
        slot_id < trace_signal_meta_.Count()) {
      return std::string(trace_signal_meta_.Name(slot_id));
    }
    return {};
  };

  for (uint32_t pid = 0; pid < num_processes_; ++pid) {
    const auto& proc = processes_[pid];

    // Primary classification from canonical suspend state.
    ProcessWaitKind kind = ProcessWaitKind::kFinished;
    SimTime target_time = 0;

    if (pid == current_running) {
      kind = ProcessWaitKind::kRunning;
    } else if (is_ready[pid] || processes_[pid].is_enqueued) {
      kind = ProcessWaitKind::kReady;
    } else if (
        pid < processes_.size() && processes_[pid].suspend_record != nullptr) {
      // Use SuspendRecord::tag as the canonical authority.
      switch (processes_[pid].suspend_record->tag) {
        case SuspendTag::kFinished:
          kind = ProcessWaitKind::kFinished;
          break;
        case SuspendTag::kDelay:
          kind = ProcessWaitKind::kSuspendedDelay;
          if (in_delay[pid]) target_time = delay_target[pid];
          break;
        case SuspendTag::kWait:
          kind = RefineWaitKind(proc.local_sub_refs, proc.global_sub_refs);
          break;
        case SuspendTag::kRepeat:
          kind = ProcessWaitKind::kSuspendedRepeat;
          break;
        case SuspendTag::kWaitEvent:
          kind = ProcessWaitKind::kSuspendedEvent;
          break;
      }
    }

    // Only include suspended processes.
    if (kind == ProcessWaitKind::kRunning || kind == ProcessWaitKind::kReady ||
        kind == ProcessWaitKind::kFinished) {
      continue;
    }

    SchedulerSnapshot::ProcessEntry entry;
    entry.process_id = pid;
    entry.wait_kind = kind;
    entry.delay_target_time = target_time;

    // Resolve process identity into the snapshot.
    if (process_meta_.IsPopulated()) {
      entry.identity = process_meta_.Format(pid);
    } else {
      entry.identity = std::format("process {}", pid);
    }

    // Collect subscription summaries for event-suspended processes.
    if (kind != ProcessWaitKind::kSuspendedDelay) {
      // Local subscriptions.
      for (const auto& ref : proc.local_sub_refs) {
        if (ref.kind == SubKind::kRebindWatcher) continue;
        if (ref.instance == nullptr) continue;

        SubscriptionSummary summary;
        summary.slot_id = ref.signal.value;
        summary.signal_name = ComposeHierarchicalTraceName(
            *ref.instance, ref.signal, *ref.instance->observability.layout);
        summary.kind = ref.kind;
        summary.edge_bucket = ref.edge_bucket;

        if (ref.kind == SubKind::kEdge) {
          const auto& groups =
              ResolveLocalSubSlot(*ref.instance, ref.signal).edge_groups;
          if (ref.edge_group < groups.size()) {
            const auto& group = groups[ref.edge_group];
            const auto& vec = (ref.edge_bucket == EdgeBucket::kPosedge)
                                  ? group.posedge_subs
                                  : group.negedge_subs;
            summary.group_last_bit = group.last_bit;
            if (ref.index < vec.size()) {
              summary.is_active = (vec[ref.index].flags & kSubActive) != 0;
            }
            const auto& imeta =
                ref.instance->observability.layout->slot_meta[ref.signal.value];
            auto storage = std::span(
                ResolveInstanceSlotBase(*ref.instance, ref.signal),
                imeta.total_bytes);
            if (!storage.empty()) {
              uint8_t byte_val = storage[group.byte_offset];
              summary.current_bit = (byte_val >> group.bit_index) & 1;
            }
          }
        } else if (ref.kind == SubKind::kChange) {
          const auto& subs =
              ResolveLocalSubSlot(*ref.instance, ref.signal).change_subs;
          if (ref.index < subs.size()) {
            summary.is_active = (subs[ref.index].flags & kSubActive) != 0;
          }
        }

        entry.subscriptions.push_back(std::move(summary));
      }

      // Global subscriptions.
      for (const auto& ref : proc.global_sub_refs) {
        if (ref.kind == SubKind::kRebindWatcher) continue;

        SubscriptionSummary summary;
        summary.slot_id = ref.signal.value;
        summary.signal_name = signal_name(ref.signal.value);
        summary.kind = ref.kind;
        summary.edge_bucket = ref.edge_bucket;

        if (ref.kind == SubKind::kEdge) {
          const auto& groups = ResolveGlobalSubSlot(ref.signal).edge_groups;
          if (ref.edge_group < groups.size()) {
            const auto& group = groups[ref.edge_group];
            const auto& vec = (ref.edge_bucket == EdgeBucket::kPosedge)
                                  ? group.posedge_subs
                                  : group.negedge_subs;
            summary.group_last_bit = group.last_bit;
            if (ref.index < vec.size()) {
              summary.is_active = (vec[ref.index].flags & kSubActive) != 0;
            }
            if (!design_state.empty() &&
                ref.signal.value < slot_meta_registry_.Size()) {
              const auto& meta = slot_meta_registry_.Get(ref.signal.value);
              auto storage = std::span(
                  ResolveSlotBase(meta, design_state_base_, const_instances_),
                  meta.total_bytes);
              if (!storage.empty()) {
                uint8_t byte_val = storage[group.byte_offset];
                summary.current_bit = (byte_val >> group.bit_index) & 1;
              }
            }
          }
        } else if (ref.kind == SubKind::kChange) {
          const auto& subs = ResolveGlobalSubSlot(ref.signal).change_subs;
          if (ref.index < subs.size()) {
            summary.is_active = (subs[ref.index].flags & kSubActive) != 0;
          }
        }

        entry.subscriptions.push_back(std::move(summary));
      }
    }

    snap.suspended_processes.push_back(std::move(entry));
  }

  return snap;
}

void Engine::RenderSchedulerSnapshot(
    FILE* sink, const SchedulerSnapshot& snapshot) {
  fmt::print(
      sink, "lyra: simulation ended: {}\n",
      EndReasonLabel(snapshot.end_reason));
  fmt::print(sink, "lyra: time={} delta={}\n", snapshot.time, snapshot.delta);
  fmt::print(
      sink, "lyra: queues: active={} inactive={} next_delta={} delay={}\n",
      snapshot.active_queue_size, snapshot.inactive_queue_size,
      snapshot.next_delta_queue_size, snapshot.delay_queue_entry_count);

  if (snapshot.suspended_processes.empty()) {
    fmt::print(sink, "lyra: no suspended processes\n");
    return;
  }

  fmt::print(
      sink, "lyra: {} suspended processes:\n",
      snapshot.suspended_processes.size());

  for (const auto& proc : snapshot.suspended_processes) {
    fmt::print(
        sink, "  [pid={}] {} -- {}\n", proc.process_id, proc.identity,
        WaitKindLabel(proc.wait_kind));

    if (proc.wait_kind == ProcessWaitKind::kSuspendedDelay) {
      int64_t remaining = static_cast<int64_t>(proc.delay_target_time) -
                          static_cast<int64_t>(snapshot.time);
      fmt::print(
          sink, "      target_time={} (in {} ticks)\n", proc.delay_target_time,
          remaining);
    }

    for (const auto& sub : proc.subscriptions) {
      std::string slot_label = std::format("slot {}", sub.slot_id);
      if (!sub.signal_name.empty()) {
        slot_label += std::format(" ({})", sub.signal_name);
      }

      if (sub.kind == SubKind::kEdge) {
        fmt::print(
            sink, "      {} {} on {} active={} last_bit={} current={}\n",
            SubKindLabel(sub.kind), EdgeBucketLabel(sub.edge_bucket),
            slot_label, sub.is_active ? "yes" : "no", sub.group_last_bit,
            sub.current_bit);
      } else {
        fmt::print(
            sink, "      {} on {} active={}\n", SubKindLabel(sub.kind),
            slot_label, sub.is_active ? "yes" : "no");
      }
    }
  }
}

}  // namespace lyra::runtime
