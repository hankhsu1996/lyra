#include "lyra/runtime/engine_scheduler.hpp"

#include <algorithm>
#include <cstdint>
#include <cstring>
#include <format>
#include <span>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/mutation_event.hpp"
#include "lyra/common/range_set.hpp"
#include "lyra/runtime/engine.hpp"
#include "lyra/runtime/engine_types.hpp"
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

}  // namespace

void Engine::ScheduleInitial(ProcessHandle handle) {
  // Skip comb kernel processes - they are evaluated inline during flush,
  // not through the normal scheduler activation path.
  if (comb_kernel_indices_.contains(handle.process_id)) return;

  active_queue_.push_back(
      ScheduledEvent{
          .handle = handle,
          .resume = ResumePoint{.block_index = 0, .instruction_index = 0},
      });
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
  delay_queue_[wake_time].push_back(
      ScheduledEvent{
          .handle = handle,
          .resume = resume,
      });
}

void Engine::DelayZero(ProcessHandle handle, ResumePoint resume) {
  inactive_queue_.push(
      ScheduledEvent{
          .handle = handle,
          .resume = resume,
      });
}

void Engine::ScheduleNextDelta(ProcessHandle handle, ResumePoint resume) {
  next_delta_queue_.push_back(
      ScheduledEvent{
          .handle = handle,
          .resume = resume,
      });
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
      value_ptr == nullptr || mask_ptr == nullptr || byte_size == 0) {
    throw common::InternalError(
        "Engine::ScheduleNba", "null pointer or zero byte_size");
  }

  ValidateSlotRootPointer(
      notify_base_ptr, notify_slot_id, slot_meta_registry_, design_state_base_,
      "Engine::ScheduleNba");

  auto val_span = std::span(static_cast<const uint8_t*>(value_ptr), byte_size);
  auto mask_span = std::span(static_cast<const uint8_t*>(mask_ptr), byte_size);
  nba_queue_.push_back(
      NbaEntry{
          .write_ptr = write_ptr,
          .notify_base_ptr = notify_base_ptr,
          .byte_size = byte_size,
          .notify_slot_id = notify_slot_id,
          .value = std::vector<uint8_t>(val_span.begin(), val_span.end()),
          .mask = std::vector<uint8_t>(mask_span.begin(), mask_span.end()),
      });
}

void Engine::ExecuteRegion(Region region) {
  switch (region) {
    case Region::kActive: {
      while (!finished_ && !active_queue_.empty()) {
        auto events = std::move(active_queue_);
        active_queue_.clear();
        for (const auto& event : events) {
          if (finished_) {
            break;
          }

          auto proc_it = process_states_.find(event.handle);
          if (proc_it != process_states_.end()) {
            proc_it->second.is_enqueued = false;
          }

          ClearProcessSubscriptions(event.handle);

          runner_(*this, event.handle, event.resume);
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
        if (entry.value.size() != entry.byte_size ||
            entry.mask.size() != entry.byte_size) {
          throw common::InternalError(
              "Engine::ExecuteRegion(kNBA)", "value/mask size mismatch");
        }
        auto target_span =
            std::span(static_cast<uint8_t*>(entry.write_ptr), entry.byte_size);
        bool changed = false;
        for (uint32_t i = 0; i < entry.byte_size; ++i) {
          uint8_t old_byte = target_span[i];
          uint8_t new_byte =
              (old_byte & ~entry.mask[i]) | (entry.value[i] & entry.mask[i]);
          if (old_byte != new_byte) {
            changed = true;
          }
          target_span[i] = new_byte;
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
  while (true) {
    while (!finished_ && (!active_queue_.empty() || !inactive_queue_.empty())) {
      ExecuteRegion(Region::kActive);
      if (!inactive_queue_.empty()) {
        ExecuteRegion(Region::kInactive);
      }
    }

    FlushAndPropagateConnections();  // Flush + connection propagation

    ExecuteRegion(Region::kNBA);

    FlushAndPropagateConnections();  // Flush + connection propagation

    if (finished_ || next_delta_queue_.empty()) {
      break;
    }
    active_queue_ = std::move(next_delta_queue_);
    next_delta_queue_.clear();
  }

  ExecutePostponedRegion();
  FlushDirtySlots();
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

  // Build trigger map: trigger_slot_id -> {start, count}
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
    std::span<const uint32_t> words, CombFunc* processes, void** states) {
  if (words.empty()) return;

  // Word table format: [num_comb, (proc_idx, num_triggers, trigger_0, ...)*]
  uint32_t pos = 0;
  uint32_t num_comb = words[pos++];

  for (uint32_t ki = 0; ki < num_comb; ++ki) {
    if (pos + 2 > words.size()) {
      throw common::InternalError(
          "Engine::InitCombKernels", "word table truncated");
    }
    uint32_t proc_idx = words[pos++];
    uint32_t num_triggers = words[pos++];
    if (pos + num_triggers > words.size()) {
      throw common::InternalError(
          "Engine::InitCombKernels", "trigger list truncated");
    }

    auto comb_idx = static_cast<uint32_t>(comb_kernels_.size());
    comb_kernels_.push_back(
        CombKernel{
            .func = processes[proc_idx],
            .state = states[proc_idx],
            .process_index = proc_idx,
        });
    comb_kernel_indices_.insert(proc_idx);

    for (uint32_t ti = 0; ti < num_triggers; ++ti) {
      uint32_t trigger_slot = words[pos++];
      comb_trigger_map_[trigger_slot].push_back(comb_idx);
    }
  }
}

void Engine::SeedCombKernelDirtyMarks() {
  for (const auto& [trigger_slot, _] : comb_trigger_map_) {
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

  // Propagate connections and comb kernels until stable, then flush
  // subscriptions. Connections and comb kernels must fire before subscription
  // wakeup so that downstream processes observe the propagated values.
  //
  // The delta accumulates across iterations: MarkSlotDirty from connection
  // writes and comb kernel stores appends to the same delta. We track how
  // many delta entries we've already scanned to process only new additions.
  constexpr uint32_t kMaxIterations = 100;
  uint32_t scanned = 0;
  auto* base = static_cast<uint8_t*>(design_state_base_);
  for (uint32_t iter = 0; iter < kMaxIterations; ++iter) {
    auto all_dirty = update_set_.DeltaDirtySlots();
    if (scanned >= all_dirty.size()) break;

    bool any_changed = false;
    auto new_dirty = all_dirty.subspan(scanned);
    scanned = static_cast<uint32_t>(all_dirty.size());

    // Phase 1: connections (pure memcpy, no side effects)
    if (has_conns) {
      for (uint32_t slot_id : new_dirty) {
        auto it = conn_trigger_map_.find(slot_id);
        if (it == conn_trigger_map_.end()) continue;
        auto [start, count] = it->second;
        for (uint32_t ci = start; ci < start + count; ++ci) {
          const auto& conn = all_connections_[ci];
          auto* src = base + conn.src_byte_offset;
          auto* dst = base + conn.dst_byte_offset;
          if (std::memcmp(dst, src, conn.byte_size) != 0) {
            std::memcpy(dst, src, conn.byte_size);
            MarkSlotDirty(conn.dst_slot_id);
            any_changed = true;
          }
        }
      }
    }

    // Phase 2: comb kernels (call compiled functions directly)
    if (has_combs) {
      for (uint32_t slot_id : new_dirty) {
        auto it = comb_trigger_map_.find(slot_id);
        if (it == comb_trigger_map_.end()) continue;
        for (uint32_t ki : it->second) {
          const auto& ck = comb_kernels_[ki];
          ck.func(ck.state, 0);
        }
      }
      // Comb kernels may have produced new dirty marks via LyraStorePacked
      auto updated_dirty = update_set_.DeltaDirtySlots();
      if (updated_dirty.size() > scanned) {
        any_changed = true;
      }
    }

    if (!any_changed) break;
  }

  // Flush subscriptions with all accumulated dirty marks (process writes +
  // connection propagation + comb kernels), then clear the delta.
  FlushSignalUpdates();
  update_set_.ClearDelta();
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

}  // namespace lyra::runtime
