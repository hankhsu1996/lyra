#include "lyra/runtime/engine_scheduler.hpp"

#include <algorithm>
#include <cstdint>
#include <cstdio>
#include <cstring>
#include <format>
#include <span>
#include <string>
#include <unistd.h>
#include <utility>
#include <variant>
#include <vector>

#include <fmt/core.h>

#include "lyra/common/diagnostic/print.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/mutation_event.hpp"
#include "lyra/common/range_set.hpp"
#include "lyra/runtime/activation_trace.hpp"
#include "lyra/runtime/activation_trace_format.hpp"
#include "lyra/runtime/engine.hpp"
#include "lyra/runtime/engine_types.hpp"
#include "lyra/runtime/loop_budget.hpp"
#include "lyra/runtime/trace_flush.hpp"

namespace lyra::runtime {

namespace {

// Fast-fail validation: notify_base_ptr must be the true slot root,
// not a projected sub-field pointer. Catches codegen bugs where
// write_ptr is passed instead of the slot base.
void ValidateSlotRootPointer(
    const void* ptr, uint32_t slot_id, const SlotMetaRegistry& registry,
    const void* design_state_base, const char* caller) {
  if (!registry.IsPopulated() || design_state_base == nullptr) {
    return;
  }
  const auto* expected = static_cast<const uint8_t*>(design_state_base) +
                         registry.Get(slot_id).base_off;
  if (static_cast<const uint8_t*>(ptr) != expected) {
    throw common::InternalError(
        caller, "pointer does not match slot root address");
  }
}

// Apply a full-overwrite NBA entry: direct compare/copy.
// Returns true if the target was changed.
auto ApplyFullOverwriteNba(const NbaEntry& entry, uint8_t* target) -> bool {
  if (entry.mask.Size() != 0) {
    throw common::InternalError(
        "ApplyFullOverwriteNba", "mask must be empty for full overwrite");
  }
  bool changed = std::memcmp(target, entry.value.Data(), entry.byte_size) != 0;
  if (changed) {
    std::memcpy(target, entry.value.Data(), entry.byte_size);
  }
  return changed;
}

// Apply a masked-merge NBA entry: per-byte (old & ~mask) | (new & mask).
// Returns true if any byte was changed.
auto ApplyMaskedMergeNba(const NbaEntry& entry, uint8_t* target) -> bool {
  if (entry.mask.Size() != entry.byte_size) {
    throw common::InternalError("ApplyMaskedMergeNba", "mask size mismatch");
  }
  const auto* val = entry.value.Data();
  const auto* mask = entry.mask.Data();
  bool changed = false;
  for (uint32_t i = 0; i < entry.byte_size; ++i) {
    uint8_t old_byte = target[i];                                   // NOLINT
    uint8_t new_byte = (old_byte & ~mask[i]) | (val[i] & mask[i]);  // NOLINT
    if (old_byte != new_byte) {
      changed = true;
    }
    target[i] = new_byte;  // NOLINT
  }
  return changed;
}

}  // namespace

void Engine::ScheduleInitial(ProcessHandle handle) {
  // Skip comb kernel processes - they are evaluated inline during flush,
  // not through the normal scheduler activation path.
  if (handle.process_id < comb_kernel_flags_.size() &&
      comb_kernel_flags_[handle.process_id] != 0) {
    return;
  }

  ScheduledEvent event{
      .handle = handle,
      .resume = ResumePoint{.block_index = 0, .instruction_index = 0},
      .cause = WakeCause::kInitial,
      .trigger_slot = kNoTriggerSlot,
  };
  active_queue_.push_back(event);
}

void Engine::Delay(ProcessHandle handle, ResumePoint resume, SimTime ticks) {
  // Checked addition to prevent overflow
  if (ticks > kNoTimeLimit - current_time_) {
    throw common::InternalError(
        "Engine::Delay", std::format(
                             "wake time overflow: current_time={} + ticks={}",
                             current_time_, ticks));
  }
  SimTime wake_time = current_time_ + ticks;
  ScheduledEvent event{
      .handle = handle,
      .resume = resume,
      .cause = WakeCause::kDelay,
      .trigger_slot = kNoTriggerSlot,
  };
  delay_queue_[wake_time].push_back(event);
}

void Engine::DelayZero(ProcessHandle handle, ResumePoint resume) {
  ScheduledEvent event{
      .handle = handle,
      .resume = resume,
      .cause = WakeCause::kDelayZero,
      .trigger_slot = kNoTriggerSlot,
  };
  inactive_queue_.push(event);
}

void Engine::ScheduleNextDelta(ProcessHandle handle, ResumePoint resume) {
  ScheduledEvent event{
      .handle = handle,
      .resume = resume,
      .cause = WakeCause::kRepeat,
      .trigger_slot = kNoTriggerSlot,
  };
  next_delta_queue_.push_back(event);
}

void Engine::SchedulePostponed(PostponedCallback callback, void* design_state) {
  postponed_queue_.push_back(
      PostponedRecord{
          .callback = callback,
          .design_state = design_state,
      });
}

void Engine::RegisterMonitor(
    MonitorCheckCallback check_thunk, void* design_state,
    const void* initial_prev, uint32_t size) {
  active_monitor_ = MonitorState{
      .enabled = true,
      .check_thunk = check_thunk,
      .design_state = design_state,
      .prev_values = {},
  };

  if (initial_prev != nullptr && size > 0) {
    auto prev_span = std::span(static_cast<const uint8_t*>(initial_prev), size);
    active_monitor_->prev_values.assign(prev_span.begin(), prev_span.end());
  } else {
    active_monitor_->prev_values.resize(size, 0);
  }
}

void Engine::SetMonitorEnabled(bool enabled) {
  if (active_monitor_.has_value()) {
    active_monitor_->enabled = enabled;
  }
}

void Engine::ScheduleNba(
    void* write_ptr, const void* notify_base_ptr, const void* value_ptr,
    const void* mask_ptr, uint32_t byte_size, uint32_t notify_slot_id) {
  if (write_ptr == nullptr || notify_base_ptr == nullptr ||
      value_ptr == nullptr || byte_size == 0) {
    throw common::InternalError(
        "Engine::ScheduleNba", "null pointer or zero byte_size");
  }

  ValidateSlotRootPointer(
      notify_base_ptr, notify_slot_id, slot_meta_registry_, design_state_base_,
      "Engine::ScheduleNba");

  NbaEntry entry;
  entry.write_ptr = write_ptr;
  entry.notify_base_ptr = notify_base_ptr;
  entry.byte_size = byte_size;
  entry.notify_slot_id = notify_slot_id;
  entry.value.AssignCopy(value_ptr, byte_size);

  if (mask_ptr != nullptr) {
    entry.mode = NbaWriteMode::kMaskedMerge;
    entry.mask.AssignCopy(mask_ptr, byte_size);
  } else {
    entry.mode = NbaWriteMode::kFullOverwrite;
  }

  nba_queue_.push_back(std::move(entry));
}

void Engine::RunOneActivation(const ScheduledEvent& event) {
  uint32_t pid = event.handle.process_id;

  // Update progress counters (signal-safe reads by SIGUSR1 handler).
  last_process_id_.store(pid, std::memory_order_release);
  activation_seq_.fetch_add(1, std::memory_order_release);
  current_running_process_.store(pid, std::memory_order_release);
  phase_.store(
      static_cast<uint32_t>(Phase::kRunProcess), std::memory_order_release);

  // Begin activation context for dirty counting.
  if (activation_trace_.has_value()) {
    activation_ctx_.active = true;
    ++activation_ctx_.generation;
    if (activation_ctx_.generation == 0) {
      ++activation_ctx_.generation;
    }
    activation_ctx_.dirty_count = 0;
  }

  // Reset loop budget before each process activation.
  LyraResetLoopBudget(kDefaultLoopBudget);

  process_dispatch_.fn(
      process_dispatch_.ctx, *this, event.handle, event.resume);

  // End activation context; emit run event.
  if (activation_trace_.has_value()) {
    activation_ctx_.active = false;
    TraceRun(event);
  }

  current_running_process_.store(UINT32_MAX, std::memory_order_release);
  phase_.store(static_cast<uint32_t>(Phase::kIdle), std::memory_order_release);
}

void Engine::ExecuteRegion(Region region) {
  switch (region) {
    case Region::kActive: {
      uint32_t active_iterations = 0;
      while (!finished_ && !active_queue_.empty()) {
        auto events = std::move(active_queue_);
        active_queue_.clear();
        for (const auto& event : events) {
          if (finished_) {
            break;
          }

          if (num_processes_ > 0) {
            if (event.handle.process_id >= num_processes_) {
              throw common::InternalError(
                  "Engine::ExecuteRegion",
                  std::format(
                      "process_id {} exceeds num_processes {}",
                      event.handle.process_id, num_processes_));
            }
            process_states_[event.handle.process_id].is_enqueued = false;
          }

          TraceWake(event);

          if (!HasPostActivationReconciliation()) {
            ClearProcessSubscriptions(event.handle);
          }
          RunOneActivation(event);
          if (!finished_ && HasPostActivationReconciliation()) {
            ReconcilePostActivation(event.handle);
          }
        }
        ++active_iterations;
        if (active_iterations % 2 == 0) {
          lyra::PrintWarning(
              std::format(
                  "active region: {} iterations at time {}, "
                  "queue has {} events",
                  active_iterations, current_time_, active_queue_.size()));
        }
        if (active_iterations >= 100) {
          lyra::PrintError("active region runaway detected, aborting");
          finished_ = true;
          break;
        }
      }
      break;
    }
    case Region::kInactive: {
      while (!inactive_queue_.empty()) {
        active_queue_.push_back(inactive_queue_.front());
        inactive_queue_.pop();
      }
      break;
    }
    case Region::kNBA: {
      if (nba_queue_.empty()) {
        break;
      }
      for (const auto& entry : nba_queue_) {
        if (entry.value.Size() != entry.byte_size) {
          throw common::InternalError(
              "Engine::ExecuteRegion(kNBA)", "value size mismatch");
        }
        auto* target = static_cast<uint8_t*>(entry.write_ptr);
        bool changed = false;

        switch (entry.mode) {
          case NbaWriteMode::kFullOverwrite:
            changed = ApplyFullOverwriteNba(entry, target);
            break;
          case NbaWriteMode::kMaskedMerge:
            changed = ApplyMaskedMergeNba(entry, target);
            break;
        }

        if (changed) {
          if (static_cast<const uint8_t*>(entry.write_ptr) <
              static_cast<const uint8_t*>(entry.notify_base_ptr)) {
            throw common::InternalError(
                "Engine::ExecuteRegion(kNBA)",
                "write_ptr before notify_base_ptr");
          }
          auto diff = static_cast<uint32_t>(
              static_cast<const uint8_t*>(entry.write_ptr) -
              static_cast<const uint8_t*>(entry.notify_base_ptr));
          MarkDirtyRange(entry.notify_slot_id, diff, entry.byte_size);
        }
      }
      nba_queue_.clear();
      break;
    }
  }
}

void Engine::ExecutePostponedRegion() {
  for (const auto& record : postponed_queue_) {
    if (finished_) break;
    record.callback(record.design_state, this);
  }
  postponed_queue_.clear();

  if (!finished_ && active_monitor_.has_value() && active_monitor_->enabled &&
      active_monitor_->check_thunk != nullptr) {
    active_monitor_->check_thunk(
        active_monitor_->design_state, this,
        active_monitor_->prev_values.data());
  }
}

void Engine::ExecuteTimeSlot() {
  current_delta_ = 0;

  while (true) {
    while (!finished_ && (!active_queue_.empty() || !inactive_queue_.empty())) {
      ExecuteRegion(Region::kActive);
      if (!inactive_queue_.empty()) {
        ExecuteRegion(Region::kInactive);
      }
    }

    phase_.store(
        static_cast<uint32_t>(Phase::kFlushUpdates), std::memory_order_release);
    FlushAndPropagateConnections();  // Flush + connection propagation

    phase_.store(
        static_cast<uint32_t>(Phase::kCommitNba), std::memory_order_release);
    ExecuteRegion(Region::kNBA);

    phase_.store(
        static_cast<uint32_t>(Phase::kFlushUpdates), std::memory_order_release);
    FlushAndPropagateConnections();  // Flush + connection propagation

    if (finished_ || next_delta_queue_.empty()) {
      break;
    }

    ++current_delta_;
    if (current_delta_ % 100 == 0) {
      lyra::PrintWarning(
          std::format(
              "time {}: {} delta cycles, {} pending in next delta",
              current_time_, current_delta_, next_delta_queue_.size()));
    }
    if (current_delta_ >= 10000) {
      lyra::PrintError(
          std::format(
              "delta cycle limit exceeded at time {}; "
              "{} processes still pending",
              current_time_, next_delta_queue_.size()));
      finished_ = true;
      break;
    }

    active_queue_ = std::move(next_delta_queue_);
    next_delta_queue_.clear();
  }

  phase_.store(
      static_cast<uint32_t>(Phase::kPostponed), std::memory_order_release);
  ExecutePostponedRegion();
  FlushDirtySlots();
  phase_.store(static_cast<uint32_t>(Phase::kIdle), std::memory_order_release);
}

auto Engine::Run(SimTime max_time) -> SimTime {
  if (trace_manager_.IsEnabled()) {
    trace_manager_.EmitTimeAdvance(current_time_);
  }

  while (!finished_ && current_time_ <= max_time) {
    ExecuteTimeSlot();

    if (finished_) {
      break;
    }

    if (delay_queue_.empty()) {
      break;
    }

    phase_.store(
        static_cast<uint32_t>(Phase::kAdvanceTime), std::memory_order_release);

    auto it = delay_queue_.begin();
    current_time_ = it->first;

    if (current_time_ > max_time) {
      break;
    }

    if (trace_manager_.IsEnabled()) {
      trace_manager_.EmitTimeAdvance(current_time_);
    }

    for (auto& event : it->second) {
      active_queue_.push_back(std::move(event));
    }
    delay_queue_.erase(it);
  }

  return current_time_;
}

void Engine::FlushDirtySlots() {
  if (trace_manager_.IsEnabled() && !update_set_.IsEmpty() &&
      design_state_base_ != nullptr) {
    FlushDirtySlotsToTrace(
        trace_manager_, slot_meta_registry_, design_state_base_, update_set_);
  }
  update_set_.Clear();
}

void Engine::InitConnectionBatch(std::span<const ConnectionDescriptor> descs) {
  if (descs.empty()) return;

  // Sort by trigger_slot_id so each group is contiguous
  struct IndexedConn {
    uint32_t trigger_slot_id;
    BatchedConnection conn;
  };
  std::vector<IndexedConn> sorted;
  sorted.reserve(descs.size());
  for (const auto& d : descs) {
    if (d.trigger_byte_size > 0) {
      ++conn_narrow_count_;
    } else {
      ++conn_full_slot_count_;
    }
    sorted.push_back(
        {d.trigger_slot_id, BatchedConnection{
                                .src_byte_offset = d.src_byte_offset,
                                .dst_byte_offset = d.dst_byte_offset,
                                .byte_size = d.byte_size,
                                .dst_slot_id = d.dst_slot_id}});
  }
  std::sort(sorted.begin(), sorted.end(), [](const auto& a, const auto& b) {
    return a.trigger_slot_id < b.trigger_slot_id;
  });

  all_connections_.reserve(sorted.size());
  for (const auto& s : sorted) {
    all_connections_.push_back(s.conn);
  }

  // Build dense trigger map: trigger_slot_id -> {start, count}
  // Sized from authoritative slot count (same universe as signal_waiters_).
  if (!slot_meta_registry_.IsPopulated()) {
    throw common::InternalError(
        "Engine::InitConnectionBatch",
        "InitConnectionBatch before InitSlotMeta");
  }
  conn_trigger_map_.resize(slot_meta_registry_.Size());

  uint32_t i = 0;
  while (i < sorted.size()) {
    uint32_t trigger = sorted[i].trigger_slot_id;
    uint32_t start = i;
    while (i < sorted.size() && sorted[i].trigger_slot_id == trigger) {
      ++i;
    }
    conn_trigger_map_[trigger] = {start, i - start};
  }
}

void Engine::EvaluateAllConnections() {
  if (all_connections_.empty() || design_state_base_ == nullptr) return;
  auto* base = static_cast<uint8_t*>(design_state_base_);
  for (const auto& conn : all_connections_) {
    auto* src = base + conn.src_byte_offset;
    auto* dst = base + conn.dst_byte_offset;
    if (std::memcmp(dst, src, conn.byte_size) != 0) {
      std::memcpy(dst, src, conn.byte_size);
      MarkSlotDirty(conn.dst_slot_id);
    }
  }
}

void Engine::InitCombKernels(
    std::span<const uint32_t> words, CombFunc* comb_funcs, void** states) {
  if (words.empty()) return;

  // Word table format:
  // [num_comb, (proc_idx, num_triggers, (slot, byte_off, byte_size), ...)*]
  uint32_t pos = 0;
  uint32_t num_comb = words[pos++];

  // Validate init order: slot meta must be populated for trigger map sizing.
  if (!slot_meta_registry_.IsPopulated()) {
    throw common::InternalError(
        "Engine::InitCombKernels", "InitCombKernels before InitSlotMeta");
  }

  // Size comb_kernel_flags_ once from authoritative process count.
  comb_kernel_flags_.resize(num_processes_, 0);

  // First pass: parse kernels, collect per-slot trigger entries.
  struct ParsedTrigger {
    uint32_t slot_id;
    uint32_t kernel_idx;
    uint32_t byte_offset;
    uint32_t byte_size;
  };
  std::vector<ParsedTrigger> entries;

  for (uint32_t ki = 0; ki < num_comb; ++ki) {
    if (pos + 2 > words.size()) {
      throw common::InternalError(
          "Engine::InitCombKernels", "word table truncated");
    }
    uint32_t proc_idx = words[pos++];
    uint32_t num_triggers = words[pos++];
    if (pos + num_triggers * 3 > words.size()) {
      throw common::InternalError(
          "Engine::InitCombKernels", "trigger list truncated");
    }

    if (proc_idx >= num_processes_) {
      throw common::InternalError(
          "Engine::InitCombKernels",
          std::format(
              "comb kernel proc_idx {} exceeds num_processes {}", proc_idx,
              num_processes_));
    }

    auto comb_idx = static_cast<uint32_t>(comb_kernels_.size());
    comb_kernels_.push_back(
        CombKernel{
            .func = comb_funcs[ki],
            .state = states[proc_idx],
            .process_index = proc_idx,
        });

    comb_kernel_flags_[proc_idx] = 1;

    for (uint32_t ti = 0; ti < num_triggers; ++ti) {
      uint32_t trigger_slot = words[pos++];
      uint32_t byte_offset = words[pos++];
      uint32_t byte_size = words[pos++];
      entries.push_back({trigger_slot, comb_idx, byte_offset, byte_size});
      if (byte_size > 0) {
        ++comb_narrow_count_;
      } else {
        ++comb_full_slot_count_;
      }
    }
  }

  if (entries.empty()) return;

  // Sort by slot_id for contiguous grouping.
  std::sort(entries.begin(), entries.end(), [](const auto& a, const auto& b) {
    return a.slot_id < b.slot_id;
  });

  // Build flat backing array and dense range table.
  // Sized from authoritative slot count (same universe as signal_waiters_).
  comb_trigger_backing_.reserve(entries.size());
  comb_trigger_map_.resize(slot_meta_registry_.Size());

  uint32_t i = 0;
  while (i < entries.size()) {
    uint32_t slot = entries[i].slot_id;
    uint32_t start = static_cast<uint32_t>(comb_trigger_backing_.size());
    while (i < entries.size() && entries[i].slot_id == slot) {
      comb_trigger_backing_.push_back({
          .kernel_idx = entries[i].kernel_idx,
          .byte_offset = entries[i].byte_offset,
          .byte_size = entries[i].byte_size,
      });
      ++i;
    }
    uint32_t count =
        static_cast<uint32_t>(comb_trigger_backing_.size()) - start;
    comb_trigger_map_[slot] = {start, count};
    comb_trigger_slots_.push_back(slot);
  }
}

void Engine::SeedCombKernelDirtyMarks() {
  for (uint32_t trigger_slot : comb_trigger_slots_) {
    MarkSlotDirty(trigger_slot);
  }
}

void Engine::FlushAndPropagateConnections() {
  bool has_conns = !all_connections_.empty();
  bool has_combs = !comb_kernels_.empty();
  if (!has_conns && !has_combs) {
    FlushSignalUpdates();
    update_set_.ClearDelta();
    return;
  }

  // Fixed-point propagation of connections and comb kernels.
  //
  // Two tracking channels serve different purposes:
  //   update_set_ (delta_dirty_/delta_seen_): cross-phase dirty tracking for
  //     scheduler wakeup and trace snapshots. Uses dedup (delta_seen_) so each
  //     slot appears at most once per delta.
  //   pending/next_pending (local work list): fixed-point propagation
  //     scheduling within this function only. Per-iteration dedup via
  //     pending_seen[] ensures each slot appears at most once per iteration,
  //     preventing exponential blowup from comb kernel intermediate writes.
  //
  // The local work list exists because delta_seen_ dedup is wrong for
  // convergence: if a comb kernel writes an intermediate value then re-writes
  // the correct value in a later iteration, the corrected write would be
  // invisible to delta_dirty_ (already deduped). The local list captures
  // comb writes via comb_write_capture_ which bypasses delta_seen_.
  //
  // Using a local vector also avoids span invalidation: iterating
  // delta_dirty_ while MarkSlotDirty pushes to it would be UB.
  constexpr uint32_t kMaxIterations = 100;
  uint32_t iterations_used = 0;
  auto* base = static_cast<uint8_t*>(design_state_base_);

  // Seed work list from current delta dirty slots.
  auto initial = update_set_.DeltaDirtySlots();
  std::vector<uint32_t> pending(initial.begin(), initial.end());

  // Scoped capture buffer for comb kernel writes. While this vector is
  // installed in comb_write_capture_, MarkSlotDirty/MarkDirtyRange push
  // written slot IDs here in addition to update_set_. This lets the
  // fixed-point loop observe comb output changes that delta_seen_ would
  // otherwise hide.
  std::vector<uint32_t> comb_writes;

  // Per-iteration dedup bitvector. Prevents exponential blowup when comb
  // kernels write intermediate values (e.g. a = f(x); a = a + g(x)) that
  // differ from the final value.
  uint32_t slot_count = slot_meta_registry_.Size();
  std::vector<uint8_t> pending_seen(slot_count, 0);

  // Helper: enqueue a slot into next_pending with dedup.
  auto enqueue_pending = [&](std::vector<uint32_t>& next, uint32_t slot_id) {
    if (slot_id < slot_count && pending_seen[slot_id] == 0) {
      pending_seen[slot_id] = 1;
      next.push_back(slot_id);
    }
  };

  // Pre-comb snapshot state for self-trigger suppression. A kernel triggered
  // by slot S may write intermediate values to S that differ from the final
  // value (e.g. a = f(x); a = a + g(x)). Both intermediate and final writes
  // pass LyraStorePacked's memcmp, but the NET change (pre-comb vs post-comb)
  // is zero. Without snapshot comparison, the slot oscillates indefinitely.
  //
  // These are scoped to this function. The snapshot_index maps slot_id to
  // snapshot buffer position for O(1) lookup.
  struct CombSnapshot {
    uint32_t buf_off;
    uint32_t base_off;
    uint32_t total_bytes;
  };
  std::vector<uint8_t> snapshot_buf;
  // Indexed by slot_id. UINT32_MAX = no snapshot for this slot.
  std::vector<uint32_t> snapshot_index(slot_count, UINT32_MAX);
  std::vector<CombSnapshot> snapshots;
  std::vector<uint32_t> snapshotted_slots;

  for (uint32_t iter = 0; iter < kMaxIterations; ++iter) {
    if (pending.empty()) break;
    ++iterations_used;
    std::vector<uint32_t> next_pending;

    // Reset seen bits for slots in the current work list.
    for (uint32_t s : pending) pending_seen[s] = 0;

    // Phase 1: connection propagation (memcmp guards actual change).
    if (has_conns) {
      for (uint32_t slot_id : pending) {
        if (slot_id >= conn_trigger_map_.size()) continue;
        auto [start, count] = conn_trigger_map_[slot_id];
        if (count == 0) continue;
        for (uint32_t ci = start; ci < start + count; ++ci) {
          ++propagation_stats_.conn_considered;
          const auto& conn = all_connections_[ci];
          auto* src = base + conn.src_byte_offset;
          auto* dst = base + conn.dst_byte_offset;
          ++propagation_stats_.conn_memcmp_executed;
          if (std::memcmp(dst, src, conn.byte_size) != 0) {
            ++propagation_stats_.conn_memcpy_executed;
            std::memcpy(dst, src, conn.byte_size);
            MarkSlotDirty(conn.dst_slot_id);
            enqueue_pending(next_pending, conn.dst_slot_id);
          }
        }
      }
    }

    // Phase 2: comb kernel evaluation.
    // Install capture so comb writes feed back into the work list.
    if (has_combs) {
      // Snapshot pending slots that have comb triggers (pre-comb state).
      snapshot_buf.clear();
      snapshots.clear();
      snapshotted_slots.clear();
      for (uint32_t slot_id : pending) {
        if (slot_id >= comb_trigger_map_.size()) continue;
        if (comb_trigger_map_[slot_id].count == 0) continue;
        const auto& meta = slot_meta_registry_.Get(slot_id);
        auto buf_off = static_cast<uint32_t>(snapshot_buf.size());
        snapshot_buf.resize(buf_off + meta.total_bytes);
        std::memcpy(
            snapshot_buf.data() + buf_off, base + meta.base_off,
            meta.total_bytes);
        snapshot_index[slot_id] = static_cast<uint32_t>(snapshots.size());
        snapshots.push_back({buf_off, meta.base_off, meta.total_bytes});
        snapshotted_slots.push_back(slot_id);
      }

      comb_writes.clear();
      comb_write_capture_ = &comb_writes;

      for (uint32_t slot_id : pending) {
        if (slot_id >= comb_trigger_map_.size()) continue;
        auto [cstart, ccount] = comb_trigger_map_[slot_id];
        if (ccount == 0) continue;

        const auto& dirty_ranges = update_set_.DeltaRangesFor(slot_id);

        for (uint32_t ci = cstart; ci < cstart + ccount; ++ci) {
          ++propagation_stats_.comb_considered;
          const auto& entry = comb_trigger_backing_[ci];

          if (entry.byte_size > 0 &&
              !dirty_ranges.Overlaps(entry.byte_offset, entry.byte_size)) {
            ++propagation_stats_.comb_skipped_range;
            continue;
          }

          ++propagation_stats_.comb_executed;
          const auto& ck = comb_kernels_[entry.kernel_idx];
          ck.func(ck.state, 0);
        }
      }

      comb_write_capture_ = nullptr;

      // Enqueue comb writes, suppressing net-zero self-triggers.
      for (uint32_t s : comb_writes) {
        if (s >= slot_count || pending_seen[s] != 0) continue;

        // If this slot was snapshotted, compare current state to pre-comb
        // state. If bytes are identical, the kernel's intermediate writes
        // produced no net change -- suppress re-enqueue.
        if (snapshot_index[s] != UINT32_MAX) {
          const auto& snap = snapshots[snapshot_index[s]];
          if (std::memcmp(
                  base + snap.base_off, snapshot_buf.data() + snap.buf_off,
                  snap.total_bytes) == 0) {
            continue;
          }
        }

        enqueue_pending(next_pending, s);
      }

      // Clear snapshot index for exactly the slots we snapshotted.
      for (uint32_t slot_id : snapshotted_slots) {
        snapshot_index[slot_id] = UINT32_MAX;
      }
    }

    pending = std::move(next_pending);
  }

  if (!pending.empty()) {
    throw common::InternalError(
        "Engine::FlushAndPropagateConnections",
        std::format(
            "convergence not reached after {} iterations "
            "({} slots still pending)",
            kMaxIterations, pending.size()));
  }

  ++propagation_stats_.propagation_calls;
  propagation_stats_.propagation_iterations += iterations_used;
  propagation_stats_.propagation_max_iterations = std::max(
      propagation_stats_.propagation_max_iterations,
      static_cast<uint64_t>(iterations_used));

  // Flush subscriptions with all accumulated dirty marks (process writes +
  // connection propagation + comb kernels), then clear the delta.
  FlushSignalUpdates();
  update_set_.ClearDelta();
}

void Engine::DumpPropagationStats(FILE* sink) const {
  const auto& s = propagation_stats_;
  fmt::print(
      sink,
      "[lyra][stats][propagation]"
      " calls={} iterations={} max_iterations={}"
      " conn_considered={} conn_memcmp={} conn_memcpy={}"
      " conn_full_slot={} conn_narrow={}"
      " comb_considered={} comb_executed={} comb_skipped_range={}"
      " comb_full_slot={} comb_narrow={}\n",
      s.propagation_calls, s.propagation_iterations,
      s.propagation_max_iterations, s.conn_considered, s.conn_memcmp_executed,
      s.conn_memcpy_executed, conn_full_slot_count_, conn_narrow_count_,
      s.comb_considered, s.comb_executed, s.comb_skipped_range,
      comb_full_slot_count_, comb_narrow_count_);
  std::fflush(sink);
}

void Engine::OnMutation(const common::MutationEvent& event) {
  if (const auto* slot = std::get_if<common::DesignSlotId>(&event.root)) {
    if (event.ranges.IsFullExtent()) {
      update_set_.MarkSlotDirty(slot->value, event.kind, event.epoch_effect);
    } else {
      for (const auto& r : event.ranges.Ranges()) {
        update_set_.MarkDirtyRange(
            slot->value, r.offset, r.size, event.kind, event.epoch_effect);
      }
    }
    return;
  }
  // HeapObjId: NYI -- future heap-level UpdateSet tracking.
}

auto Engine::FormatProcess(uint32_t process_id) const -> std::string {
  if (process_meta_.IsPopulated()) {
    return process_meta_.Format(process_id);
  }
  return std::format("<process {}>", process_id);
}

namespace {

// Async-signal-safe: write a string literal to fd.
void SafeWrite(int fd, const char* str, size_t len) {
  // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-array-to-pointer-decay)
  static_cast<void>(write(fd, str, len));
}

// Async-signal-safe: write a uint64 as decimal to fd.
void SafeWriteU64(int fd, uint64_t val) {
  // NOLINTNEXTLINE(cppcoreguidelines-avoid-c-arrays)
  char buf[20];
  int pos = 19;
  if (val == 0) {
    buf[pos] = '0';
    SafeWrite(fd, &buf[pos], 1);  // NOLINT
    return;
  }
  while (val > 0) {
    // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-constant-array-index)
    buf[pos--] = static_cast<char>('0' + (val % 10));
    val /= 10;
  }
  // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-pointer-arithmetic)
  SafeWrite(fd, &buf[pos + 1], static_cast<size_t>(19 - pos));  // NOLINT
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

auto PhaseLabel(uint32_t phase) -> const char* {
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
  const char* label = PhaseLabel(phase);
  size_t label_len = 0;
  while (label[label_len] != '\0') ++label_len;  // NOLINT
  SafeWrite(fd, label, label_len);

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

void Engine::HandleTrap(uint32_t process_id, const TrapPayload& payload) {
  switch (payload.reason) {
    case TrapReason::kLoopBudgetExceeded: {
      std::string proc_str = FormatProcess(process_id);
      std::string loc_str;
      if (loop_site_meta_.IsPopulated() && payload.a < loop_site_meta_.Size()) {
        loc_str = loop_site_meta_.Format(payload.a);
      }

      if (loc_str.empty()) {
        lyra::PrintError(
            std::format("loop iteration limit exceeded in {}", proc_str));
      } else {
        lyra::PrintError(
            std::format(
                "loop iteration limit exceeded in {} at {}", proc_str,
                loc_str));
      }

      finished_ = true;
      break;
    }
    case TrapReason::kUserFatal:
    case TrapReason::kInternalError:
      lyra::PrintError(std::format("trap in {}", FormatProcess(process_id)));
      finished_ = true;
      break;
  }
}

void Engine::NoteActivationDirty(uint32_t slot_id) {
  if (slot_id >= activation_slot_gen_.size()) return;
  if (activation_slot_gen_[slot_id] != activation_ctx_.generation) {
    activation_slot_gen_[slot_id] = activation_ctx_.generation;
    ++activation_ctx_.dirty_count;
  }
}

void Engine::TraceWake(const ScheduledEvent& event) {
  if (!activation_trace_.has_value()) return;
  ActivationEvent ae{
      .time = current_time_,
      .delta = current_delta_,
      .process_id = event.handle.process_id,
      .trigger_slot = event.trigger_slot,
      .resume_block = event.resume.block_index,
      .kind = ActivationEventKind::kWake,
      .cause = event.cause,
      .slots_dirtied = 0,
  };
  activation_trace_->Append(ae);
  fmt::print(stderr, "{}\n", FormatActivationEvent(ae, process_meta_));
}

void Engine::TraceRun(const ScheduledEvent& event) {
  if (!activation_trace_.has_value()) return;
  ActivationEvent ae{
      .time = current_time_,
      .delta = current_delta_,
      .process_id = event.handle.process_id,
      .trigger_slot = event.trigger_slot,
      .resume_block = event.resume.block_index,
      .kind = ActivationEventKind::kRun,
      .cause = event.cause,
      .slots_dirtied = activation_ctx_.dirty_count,
  };
  activation_trace_->Append(ae);
  fmt::print(stderr, "{}\n", FormatActivationEvent(ae, process_meta_));
}

}  // namespace lyra::runtime
