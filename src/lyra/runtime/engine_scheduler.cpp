#include "lyra/runtime/engine_scheduler.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <format>
#include <span>
#include <utility>
#include <vector>

#include "lyra/common/internal_error.hpp"
#include "lyra/runtime/engine.hpp"
#include "lyra/runtime/engine_types.hpp"
#include "lyra/runtime/trace_flush.hpp"

namespace lyra::runtime {

void Engine::ScheduleInitial(ProcessHandle handle) {
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

  auto [it, inserted] =
      slot_base_ptrs_.emplace(notify_slot_id, notify_base_ptr);
  if (!inserted && it->second != notify_base_ptr) {
    throw common::InternalError(
        "Engine::ScheduleNba",
        "slot_id mapped to different base_ptr (codegen bug)");
  }

  auto val_span = std::span(static_cast<const uint8_t*>(value_ptr), byte_size);
  auto mask_span = std::span(static_cast<const uint8_t*>(mask_ptr), byte_size);
  nba_queue_.push_back(
      NbaEntry{
          .write_ptr = write_ptr,
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

      std::vector<uint32_t> unique_slots;
      unique_slots.reserve(nba_queue_.size());
      for (const auto& entry : nba_queue_) {
        unique_slots.push_back(entry.notify_slot_id);
      }
      std::ranges::sort(unique_slots);
      auto [erase_begin, erase_end] = std::ranges::unique(unique_slots);
      unique_slots.erase(erase_begin, erase_end);

      std::vector<uint8_t> old_bit0(unique_slots.size());
      for (size_t i = 0; i < unique_slots.size(); ++i) {
        const auto* base =
            static_cast<const uint8_t*>(slot_base_ptrs_.at(unique_slots[i]));
        old_bit0[i] = (*base & 1) != 0 ? 1 : 0;
      }

      std::vector<uint8_t> slot_changed(unique_slots.size(), 0);
      for (const auto& entry : nba_queue_) {
        if (entry.value.size() != entry.byte_size ||
            entry.mask.size() != entry.byte_size) {
          throw common::InternalError(
              "Engine::ExecuteRegion(kNBA)",
              "value/mask size mismatch with byte_size");
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
          auto slot_it =
              std::ranges::lower_bound(unique_slots, entry.notify_slot_id);
          if (slot_it == unique_slots.end() ||
              *slot_it != entry.notify_slot_id) {
            throw common::InternalError(
                "Engine::ExecuteRegion(kNBA)",
                "notify_slot_id not found in unique_slots");
          }
          slot_changed[slot_it - unique_slots.begin()] = 1;
        }
      }
      nba_queue_.clear();

      for (size_t i = 0; i < unique_slots.size(); ++i) {
        if (slot_changed[i] == 0) {
          continue;
        }
        const auto* base =
            static_cast<const uint8_t*>(slot_base_ptrs_.at(unique_slots[i]));
        bool new_bit0 = (*base & 1) != 0;
        RecordSignalUpdate(unique_slots[i], old_bit0[i] != 0, new_bit0, true);
        MarkSlotDirty(unique_slots[i]);
      }
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

    FlushSignalUpdates();  // Flush blocking assignment edges

    ExecuteRegion(Region::kNBA);

    FlushSignalUpdates();  // Flush NBA edges

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

}  // namespace lyra::runtime
