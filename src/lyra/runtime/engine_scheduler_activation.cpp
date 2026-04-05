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
#include "lyra/runtime/iteration_limit.hpp"
#include "lyra/runtime/trace_flush.hpp"

namespace lyra::runtime {

void Engine::ScheduleInitial(ProcessHandle handle) {
  // Skip comb kernel processes - they are evaluated inline during flush,
  // not through the normal scheduler activation path.
  if (handle.process_id < comb_kernel_flags_.size() &&
      comb_kernel_flags_[handle.process_id] != 0) {
    return;
  }

  active_queue_.push_back(
      {handle.process_id, handle.instance_id, /*resume_block=*/0});
  if (activation_trace_.has_value()) {
    wake_trace_[handle.process_id] = {
        .cause = WakeCause::kInitial, .trigger_slot = kNoTriggerSlot};
  }
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
      {handle.process_id, handle.instance_id, resume.block_index});
  if (activation_trace_.has_value()) {
    wake_trace_[handle.process_id] = {
        .cause = WakeCause::kDelay, .trigger_slot = kNoTriggerSlot};
  }
}

void Engine::DelayZero(ProcessHandle handle, ResumePoint resume) {
  inactive_queue_.push(
      {handle.process_id, handle.instance_id, resume.block_index});
  if (activation_trace_.has_value()) {
    wake_trace_[handle.process_id] = {
        .cause = WakeCause::kDelayZero, .trigger_slot = kNoTriggerSlot};
  }
}

void Engine::ScheduleNextDelta(ProcessHandle handle, ResumePoint resume) {
  next_delta_queue_.push_back(
      {handle.process_id, handle.instance_id, resume.block_index});
  if (activation_trace_.has_value()) {
    wake_trace_[handle.process_id] = {
        .cause = WakeCause::kRepeat, .trigger_slot = kNoTriggerSlot};
  }
}

void Engine::RegisterStrobe(
    StrobeProgramFn program, void* design_state,
    const ObserverContext& context) {
  postponed_queue_.push_back(
      StrobeRecord{
          .program = program,
          .design_state = design_state,
          .context = context,
      });
}

void Engine::RegisterMonitor(
    MonitorCheckProgramFn program, void* design_state,
    const ObserverContext& context, const void* initial_prev, uint32_t size) {
  active_monitor_ = MonitorRecord{
      .enabled = true,
      .program = program,
      .design_state = design_state,
      .context = context,
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

void Engine::RunOneActivation(const WakeupEntry& entry) {
  uint32_t pid = entry.process_id;

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

  // Reset iteration limit before each process activation.
  // LyraGetIterationLimit() returns the process-global configured limit.
  // 0 = unlimited: set counter to UINT32_MAX so the guard never fires.
  uint32_t limit = LyraGetIterationLimit();
  LyraResetIterationLimit(limit > 0 ? limit : UINT32_MAX);

  ProcessHandle handle{entry.process_id, entry.instance_id};
  ResumePoint resume{entry.resume_block, 0};
  process_dispatch_.fn(process_dispatch_.ctx, *this, handle, resume);

  // End activation context.
  activation_ctx_.active = false;
  if (activation_ctx_.dirty_count == 0) {
    ++stats_.core.activations_nba_only;
  }
  if (detailed_stats_enabled_) {
    auto& ps = per_process_stats_[pid];
    ++ps.runs;
    ps.total_slots_dirtied += activation_ctx_.dirty_count;
    if (activation_ctx_.dirty_count == 0) {
      ++ps.nba_only_runs;
    }
  }
  if (activation_trace_.has_value()) {
    TraceRun(entry);
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
    auto entries = std::move(active_queue_);
    active_queue_.clear();
    for (const auto& entry : entries) {
      if (finished_) {
        break;
      }

      if (num_processes_ > 0) {
        if (entry.process_id >= num_processes_) {
          throw common::InternalError(
              "Engine::ExecuteRegion",
              std::format(
                  "process_id {} exceeds num_processes {}", entry.process_id,
                  num_processes_));
        }
        process_states_[entry.process_id].is_enqueued = false;
      }

      if (activation_trace_.has_value()) {
        TraceWake(entry);
      }

      // Flush deferred assertion state on resume from wait/delay.
      // Initial activation and kRepeat use resume_block=0;
      // wait/delay resume uses the non-zero block set by LyraSuspendWait/Delay.
      if (entry.resume_block != 0) {
        FlushDeferredAssertionsForProcess(
            ProcessId::FromIndex(entry.process_id));
      }

      ProcessHandle handle{entry.process_id, entry.instance_id};
      if (!HasPostActivationReconciliation()) {
        ClearProcessSubscriptions(handle);
      }
      RunOneActivation(entry);
      if (!finished_ && HasPostActivationReconciliation()) {
        ReconcilePostActivation(handle);
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
    record.program(record.design_state, this, &record.context);
  }
  postponed_queue_.clear();

  if (!finished_ && active_monitor_.has_value() && active_monitor_->enabled &&
      active_monitor_->program != nullptr) {
    active_monitor_->program(
        active_monitor_->design_state, this, &active_monitor_->context,
        active_monitor_->prev_values.data());
  }
}

void Engine::ExecuteTimeSlot() {
  current_timeslot_epoch_ = current_timeslot_epoch_.Next();
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
    FlushAndPropagateConnections();

    phase_.store(
        static_cast<uint32_t>(Phase::kCommitNba), std::memory_order_release);
    ExecuteRegion(Region::kNBA);

    phase_.store(
        static_cast<uint32_t>(Phase::kFlushUpdates), std::memory_order_release);
    FlushAndPropagateConnections();

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
      end_reason_ = SimulationEndReason::kDeltaCycleLimit;
      break;
    }

    active_queue_.clear();
    active_queue_.swap(next_delta_queue_);
  }

  // Settle-complete: mature deferred assertions then validate decisions.
  phase_.store(
      static_cast<uint32_t>(Phase::kSettleComplete), std::memory_order_release);
  MatureAndExecuteObservedDeferredAssertions();
  RunSettleCompleteChecks();

  phase_.store(
      static_cast<uint32_t>(Phase::kPostponed), std::memory_order_release);
  ExecutePostponedRegion();
  FlushDirtySlots();
  phase_.store(static_cast<uint32_t>(Phase::kIdle), std::memory_order_release);
}

auto Engine::Run(SimTime max_time) -> SimTime {
  // end_reason_ defaults to kEmptyQueues; set to kMaxTimeReached or kFinish
  // at the point where the reason becomes known.
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
      end_reason_ = SimulationEndReason::kMaxTimeReached;
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
      FlushGlobalDirtySlotsToTrace(
          trace_manager_, slot_meta_registry_, design_state_base_,
          const_instances_, update_set_, trace_selection_, global_slot_count_);
    }
    FlushLocalDirtySlotsToTrace(trace_manager_, instances_);
  }
  update_set_.Clear();
  ClearLocalUpdates();
}

void Engine::HandleTrap(uint32_t process_id, const TrapPayload& payload) {
  switch (payload.reason) {
    case TrapReason::kIterationLimitExceeded: {
      std::string proc_str = FormatProcess(process_id);
      std::string loc_str;
      if (back_edge_site_meta_.IsPopulated() &&
          payload.a < back_edge_site_meta_.Size()) {
        loc_str = back_edge_site_meta_.Format(payload.a);
      }

      if (loc_str.empty()) {
        lyra::PrintError(
            std::format("iteration limit exceeded in {}", proc_str));
      } else {
        lyra::PrintError(
            std::format(
                "iteration limit exceeded in {} at {}", proc_str, loc_str));
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
