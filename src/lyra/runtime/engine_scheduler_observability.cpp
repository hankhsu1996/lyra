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
#include "lyra/runtime/process_meta.hpp"

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
      " conn_full_slot={} conn_narrow={}"
      " comb_full_slot={} comb_narrow={}\n",
      c.total_activations, c.activations_nba_only, c.propagation_calls,
      c.propagation_iterations, c.propagation_max_iterations, c.nba_entries,
      c.nba_elided, c.nba_changed, conn_full_slot_count_, conn_narrow_count_,
      comb_full_slot_count_, comb_narrow_count_);

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
      std::iota(order.begin(), order.end(), 0U);
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

}  // namespace lyra::runtime
