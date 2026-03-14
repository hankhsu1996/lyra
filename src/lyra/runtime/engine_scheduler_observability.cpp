#include <array>
#include <cstdint>
#include <cstdio>
#include <span>
#include <string_view>
#include <unistd.h>

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
