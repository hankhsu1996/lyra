#include <cstdint>
#include <format>
#include <span>
#include <string>
#include <utility>
#include <variant>

#include "lyra/common/diagnostic/print.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/mutation_event.hpp"
#include "lyra/runtime/engine.hpp"
#include "lyra/runtime/engine_scheduler.hpp"
#include "lyra/runtime/engine_types.hpp"
#include "lyra/runtime/loop_budget.hpp"
#include "lyra/runtime/trace_flush.hpp"

namespace lyra::runtime {

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

void Engine::RunOneActivation(const ScheduledEvent& event) {
  uint32_t pid = event.handle.process_id;

  // Update progress counters (signal-safe reads by SIGUSR1 handler).
  last_process_id_.store(pid, std::memory_order_release);
  activation_seq_.fetch_add(1, std::memory_order_release);
  current_running_process_.store(pid, std::memory_order_release);
  phase_.store(
      static_cast<uint32_t>(Phase::kRunProcess), std::memory_order_release);

  ++stats_.core.total_activations;

  // Begin activation context for dirty counting (unconditional).
  activation_ctx_.active = true;
  ++activation_ctx_.generation;
  if (activation_ctx_.generation == 0) {
    ++activation_ctx_.generation;
  }
  activation_ctx_.dirty_count = 0;

  // Reset loop budget before each process activation.
  LyraResetLoopBudget(kDefaultLoopBudget);

  process_dispatch_.fn(
      process_dispatch_.ctx, *this, event.handle, event.resume);

  // End activation context.
  activation_ctx_.active = false;
  if (activation_ctx_.dirty_count == 0) {
    ++stats_.core.activations_no_write;
  }
  if (activation_trace_.has_value()) {
    TraceRun(event);
  }

  current_running_process_.store(UINT32_MAX, std::memory_order_release);
  phase_.store(static_cast<uint32_t>(Phase::kIdle), std::memory_order_release);
}

void Engine::ExecuteRegion(Region region) {
  switch (region) {
    case Region::kActive:
      ExecuteActiveRegion();
      break;
    case Region::kInactive:
      ExecuteInactiveRegion();
      break;
    case Region::kNBA:
      ExecuteNbaRegion();
      break;
  }
}

void Engine::ExecuteActiveRegion() {
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

      if (activation_trace_.has_value()) {
        TraceWake(event);
      }

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
}

void Engine::ExecuteInactiveRegion() {
  while (!inactive_queue_.empty()) {
    active_queue_.push_back(inactive_queue_.front());
    inactive_queue_.pop();
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

    active_queue_.clear();
    active_queue_.swap(next_delta_queue_);
  }

  phase_.store(
      static_cast<uint32_t>(Phase::kPostponed), std::memory_order_release);
  ExecutePostponedRegion();
  FlushDirtySlots();
  phase_.store(static_cast<uint32_t>(Phase::kIdle), std::memory_order_release);
}

auto Engine::Run(SimTime max_time) -> SimTime {
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

    for (auto& event : it->second) {
      active_queue_.push_back(std::move(event));
    }
    delay_queue_.erase(it);
  }

  return current_time_;
}

void Engine::FlushDirtySlots() {
  if (trace_manager_.IsEnabled()) {
    trace_manager_.EmitTimeAdvance(current_time_, current_delta_);
    if (!update_set_.IsEmpty() && design_state_base_ != nullptr) {
      FlushDirtySlotsToTrace(
          trace_manager_, slot_meta_registry_, design_state_base_, update_set_,
          trace_selection_);
    }
  }
  update_set_.Clear();
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

}  // namespace lyra::runtime
