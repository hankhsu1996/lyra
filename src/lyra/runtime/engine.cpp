#include "lyra/runtime/engine.hpp"

#include <algorithm>
#include <cstdint>
#include <span>
#include <utility>
#include <vector>

#include "lyra/common/edge_kind.hpp"
#include "lyra/common/internal_error.hpp"

namespace lyra::runtime {

void Engine::ScheduleInitial(ProcessHandle handle) {
  active_queue_.push_back(
      ScheduledEvent{
          .handle = handle,
          .resume = ResumePoint{.block_index = 0, .instruction_index = 0},
      });
}

void Engine::Delay(ProcessHandle handle, ResumePoint resume, SimTime ticks) {
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

void Engine::Subscribe(
    ProcessHandle handle, ResumePoint resume, SignalId signal,
    common::EdgeKind edge) {
  waiters_[signal].push_back(
      Waiter{
          .handle = handle,
          .resume = resume,
          .edge = edge,
      });
}

void Engine::ScheduleNextDelta(ProcessHandle handle, ResumePoint resume) {
  pending_queue_.push_back(
      ScheduledEvent{
          .handle = handle,
          .resume = resume,
      });
}

void Engine::ScheduleNba(
    void* write_ptr, const void* notify_base_ptr, const void* value_ptr,
    const void* mask_ptr, uint32_t byte_size, uint32_t notify_slot_id) {
  if (write_ptr == nullptr || notify_base_ptr == nullptr ||
      value_ptr == nullptr || mask_ptr == nullptr || byte_size == 0) {
    throw common::InternalError(
        "Engine::ScheduleNba", "null pointer or zero byte_size");
  }

  // Register slot base pointer (lazily, first call per slot teaches the engine)
  auto [it, inserted] =
      slot_base_ptrs_.emplace(notify_slot_id, notify_base_ptr);
  if (!inserted && it->second != notify_base_ptr) {
    throw common::InternalError(
        "Engine::ScheduleNba",
        "slot_id mapped to different base_ptr (codegen bug)");
  }

  // Copy value and mask bytes into a new NbaEntry
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

void Engine::NotifyChange(
    SignalId signal, bool old_lsb, bool new_lsb, bool value_changed) {
  if (!value_changed) {
    return;
  }

  auto it = waiters_.find(signal);
  if (it == waiters_.end()) {
    return;
  }

  std::vector<Waiter> remaining;
  for (const auto& waiter : it->second) {
    bool triggered = false;
    switch (waiter.edge) {
      case common::EdgeKind::kPosedge:
        triggered = !old_lsb && new_lsb;
        break;
      case common::EdgeKind::kNegedge:
        triggered = old_lsb && !new_lsb;
        break;
      case common::EdgeKind::kAnyChange:
        triggered = true;
        break;
    }

    if (triggered) {
      pending_queue_.push_back(
          ScheduledEvent{
              .handle = waiter.handle,
              .resume = waiter.resume,
          });
    } else {
      remaining.push_back(waiter);
    }
  }

  if (remaining.empty()) {
    waiters_.erase(it);
  } else {
    it->second = std::move(remaining);
  }
}

void Engine::ExecuteRegion(Region region) {
  switch (region) {
    case Region::kActive: {
      // Execute all active events (may add to inactive/nba queues)
      while (!finished_ && !active_queue_.empty()) {
        auto events = std::move(active_queue_);
        active_queue_.clear();
        for (const auto& event : events) {
          if (finished_) {
            break;
          }
          runner_(*this, event.handle, event.resume);
        }
      }
      break;
    }
    case Region::kInactive: {
      // Move all inactive events to active queue
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

      // Phase 1: Gather unique slot IDs, snapshot bit0
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

      // Phase 2: Apply all masked writes, track which slots changed
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

      // Phase 3: Notify changed slots
      for (size_t i = 0; i < unique_slots.size(); ++i) {
        if (slot_changed[i] == 0) {
          continue;
        }
        const auto* base =
            static_cast<const uint8_t*>(slot_base_ptrs_.at(unique_slots[i]));
        bool new_bit0 = (*base & 1) != 0;
        NotifyChange(unique_slots[i], old_bit0[i] != 0, new_bit0, true);
      }
      break;
    }
  }
}

void Engine::ExecuteTimeSlot() {
  // IEEE 1800 stratified event scheduler: Active → Inactive → NBA per delta.
  // Each iteration of the outer loop is one delta cycle.
  while (true) {
    // Active + Inactive loop (repeat until both drained or $finish called)
    while (!finished_ && (!active_queue_.empty() || !inactive_queue_.empty())) {
      ExecuteRegion(Region::kActive);
      if (!inactive_queue_.empty()) {
        ExecuteRegion(Region::kInactive);
      }
    }

    // Commit any already-enqueued NBAs even if finished_ is set.
    ExecuteRegion(Region::kNBA);

    // Stop after this delta if $finish was called or no more work
    if (finished_ || pending_queue_.empty()) {
      break;
    }
    active_queue_ = std::move(pending_queue_);
    pending_queue_.clear();
  }
}

auto Engine::Run(SimTime max_time) -> SimTime {
  while (!finished_ && current_time_ <= max_time) {
    // Execute current time slot
    ExecuteTimeSlot();

    if (finished_) {
      break;
    }

    // Advance to next scheduled time
    if (delay_queue_.empty()) {
      break;  // No more events
    }

    auto it = delay_queue_.begin();
    current_time_ = it->first;

    if (current_time_ > max_time) {
      break;
    }

    // Move scheduled events to active queue
    for (auto& event : it->second) {
      active_queue_.push_back(std::move(event));
    }
    delay_queue_.erase(it);
  }

  return current_time_;
}

}  // namespace lyra::runtime
