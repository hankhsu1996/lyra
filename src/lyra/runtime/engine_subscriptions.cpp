#include "lyra/runtime/engine_subscriptions.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <format>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include <fmt/core.h>

#include "lyra/common/bit_target_mapping.hpp"
#include "lyra/common/edge_kind.hpp"
#include "lyra/common/index_plan.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/runtime/dyn_array_data.hpp"
#include "lyra/runtime/engine.hpp"
#include "lyra/runtime/engine_scheduler.hpp"
#include "lyra/runtime/engine_types.hpp"
#include "lyra/runtime/index_plan.hpp"
#include "lyra/runtime/slot_meta.hpp"
#include "lyra/runtime/suspend_record.hpp"
#include "lyra/runtime/update_set.hpp"
#include "lyra/runtime/wait_site.hpp"

namespace lyra::runtime {

auto Engine::AllocEdgeCold() -> uint32_t {
  if (!edge_cold_free_list_.empty()) {
    uint32_t idx = edge_cold_free_list_.back();
    edge_cold_free_list_.pop_back();
    edge_cold_pool_[idx] = EdgeTargetCold{};
    return idx;
  }
  auto idx = static_cast<uint32_t>(edge_cold_pool_.size());
  edge_cold_pool_.emplace_back();
  return idx;
}

void Engine::FreeEdgeCold(uint32_t idx) {
  edge_cold_pool_[idx] = EdgeTargetCold{};
  edge_cold_free_list_.push_back(idx);
}

auto Engine::AllocChangeCold() -> uint32_t {
  if (!change_cold_free_list_.empty()) {
    uint32_t idx = change_cold_free_list_.back();
    change_cold_free_list_.pop_back();
    change_cold_pool_[idx] = ChangeSnapshotCold{};
    return idx;
  }
  auto idx = static_cast<uint32_t>(change_cold_pool_.size());
  change_cold_pool_.emplace_back();
  return idx;
}

void Engine::FreeChangeCold(uint32_t idx) {
  change_cold_pool_[idx].snapshot.clear();
  change_cold_pool_[idx].snapshot.shrink_to_fit();
  change_cold_free_list_.push_back(idx);
}

auto Engine::AllocWatcherCold() -> uint32_t {
  if (!watcher_cold_free_list_.empty()) {
    uint32_t idx = watcher_cold_free_list_.back();
    watcher_cold_free_list_.pop_back();
    watcher_cold_pool_[idx] = WatcherCold{};
    return idx;
  }
  auto idx = static_cast<uint32_t>(watcher_cold_pool_.size());
  watcher_cold_pool_.emplace_back();
  return idx;
}

void Engine::FreeWatcherCold(uint32_t idx) {
  watcher_cold_pool_[idx].snapshot.clear();
  watcher_cold_pool_[idx].snapshot.shrink_to_fit();
  watcher_cold_free_list_.push_back(idx);
}

auto Engine::AllocContainerCold() -> uint32_t {
  if (!container_cold_free_list_.empty()) {
    uint32_t idx = container_cold_free_list_.back();
    container_cold_free_list_.pop_back();
    container_cold_pool_[idx] = ContainerCold{};
    return idx;
  }
  auto idx = static_cast<uint32_t>(container_cold_pool_.size());
  container_cold_pool_.emplace_back();
  return idx;
}

void Engine::FreeContainerCold(uint32_t idx) {
  container_cold_pool_[idx] = ContainerCold{};
  container_cold_free_list_.push_back(idx);
}

auto Engine::AllocEdgeTarget(EdgeTargetHandle handle) -> uint32_t {
  if (!edge_target_free_list_.empty()) {
    uint32_t id = edge_target_free_list_.back();
    edge_target_free_list_.pop_back();
    edge_target_table_[id] = handle;
    return id;
  }
  auto id = static_cast<uint32_t>(edge_target_table_.size());
  edge_target_table_.push_back(handle);
  return id;
}

void Engine::FreeEdgeTarget(uint32_t id) {
  edge_target_table_[id] =
      EdgeTargetHandle{UINT32_MAX, SubKind::kEdge, UINT32_MAX};
  edge_target_free_list_.push_back(id);
}

auto Engine::EvaluateEdge(common::EdgeKind edge, bool old_lsb, bool new_lsb)
    -> bool {
  switch (edge) {
    case common::EdgeKind::kPosedge:
      return !old_lsb && new_lsb;
    case common::EdgeKind::kNegedge:
      return old_lsb && !new_lsb;
    case common::EdgeKind::kAnyChange:
      return true;
  }
  return false;
}

auto Engine::CheckSubscriptionLimits(const ProcessState& proc_state) -> bool {
  if (max_total_subscriptions_ > 0 &&
      live_subscription_count_ >= max_total_subscriptions_) {
    TerminateWithResourceError(
        "global subscription limit exceeded", live_subscription_count_,
        max_total_subscriptions_);
    return false;
  }

  if (max_subscriptions_per_process_ > 0 &&
      proc_state.subscription_count >= max_subscriptions_per_process_) {
    TerminateWithResourceError(
        "per-process subscription limit exceeded",
        proc_state.subscription_count, max_subscriptions_per_process_);
    return false;
  }

  return true;
}

void Engine::TerminateWithResourceError(
    std::string_view reason, size_t current, size_t limit) {
  termination_reason_ = std::format("{} ({}/{})", reason, current, limit);
  finished_ = true;

  fmt::println(
      stderr,
      "lyra: simulation terminated: {}\n"
      "  time: {}",
      *termination_reason_, current_time_);

  PrintTopWaiters(5);
}

void Engine::PrintTopWaiters(size_t count) {
  std::vector<std::pair<SignalId, size_t>> waiter_counts;

  for (uint32_t signal = 0; signal < signal_subs_.size(); ++signal) {
    const auto& slot = signal_subs_[signal];
    size_t n = slot.edge_subs.size() + slot.change_subs.size() +
               slot.rebind_subs.size() + slot.container_subs.size();
    if (n == 0) continue;
    waiter_counts.emplace_back(signal, n);
  }

  auto middle = waiter_counts.begin() + static_cast<std::ptrdiff_t>(std::min(
                                            count, waiter_counts.size()));
  std::ranges::partial_sort(
      waiter_counts, middle,
      [](const auto& a, const auto& b) { return a.second > b.second; });

  fmt::println(stderr, "  top {} signals by waiter count:", count);
  for (size_t i = 0; i < std::min(count, waiter_counts.size()); ++i) {
    fmt::println(
        stderr, "    signal {}: {} waiters", waiter_counts[i].first,
        waiter_counts[i].second);
  }
}

void Engine::RemoveEdgeSub(uint32_t slot_id, uint32_t index) {
  auto& vec = signal_subs_[slot_id].edge_subs;
  auto& sub = vec[index];

  // Free cold entry if present.
  if (sub.cold_idx != UINT32_MAX) {
    auto& cold = edge_cold_pool_[sub.cold_idx];
    if (cold.edge_target_id != UINT32_MAX) {
      FreeEdgeTarget(cold.edge_target_id);
    }
    FreeEdgeCold(sub.cold_idx);
  }

  // Swap-and-pop.
  uint32_t last = static_cast<uint32_t>(vec.size()) - 1;
  if (index != last) {
    vec[index] = vec[last];
    // Update moved element's owning SubRef.
    auto& moved = vec[index];
    auto& moved_proc = process_states_[moved.process_id];
    moved_proc.sub_refs[moved.process_sub_idx].index = index;
    // Update edge_target_table_ if moved element is a rebind target.
    if (moved.cold_idx != UINT32_MAX) {
      auto& moved_cold = edge_cold_pool_[moved.cold_idx];
      if (moved_cold.edge_target_id != UINT32_MAX) {
        edge_target_table_[moved_cold.edge_target_id].index = index;
      }
    }
  }
  vec.pop_back();
}

void Engine::RemoveChangeSub(uint32_t slot_id, uint32_t index) {
  auto& vec = signal_subs_[slot_id].change_subs;
  auto& sub = vec[index];

  if (sub.cold_idx != UINT32_MAX) {
    FreeChangeCold(sub.cold_idx);
  }

  uint32_t last = static_cast<uint32_t>(vec.size()) - 1;
  if (index != last) {
    vec[index] = vec[last];
    auto& moved = vec[index];
    auto& moved_proc = process_states_[moved.process_id];
    moved_proc.sub_refs[moved.process_sub_idx].index = index;
  }
  vec.pop_back();
}

void Engine::RemoveRebindWatcherSub(uint32_t slot_id, uint32_t index) {
  auto& vec = signal_subs_[slot_id].rebind_subs;
  auto& sub = vec[index];

  if (sub.cold_idx != UINT32_MAX) {
    FreeWatcherCold(sub.cold_idx);
  }

  uint32_t last = static_cast<uint32_t>(vec.size()) - 1;
  if (index != last) {
    vec[index] = vec[last];
    auto& moved = vec[index];
    auto& moved_proc = process_states_[moved.process_id];
    moved_proc.sub_refs[moved.process_sub_idx].index = index;
  }
  vec.pop_back();
}

void Engine::RemoveContainerSub(uint32_t slot_id, uint32_t index) {
  auto& vec = signal_subs_[slot_id].container_subs;
  auto& sub = vec[index];

  if (sub.cold_idx != UINT32_MAX) {
    auto& cold = container_cold_pool_[sub.cold_idx];
    if (cold.edge_target_id != UINT32_MAX) {
      FreeEdgeTarget(cold.edge_target_id);
    }
    FreeContainerCold(sub.cold_idx);
  }

  uint32_t last = static_cast<uint32_t>(vec.size()) - 1;
  if (index != last) {
    vec[index] = vec[last];
    auto& moved = vec[index];
    auto& moved_proc = process_states_[moved.process_id];
    moved_proc.sub_refs[moved.process_sub_idx].index = index;
    // Update edge_target_table_ if moved element is a rebind target.
    if (moved.cold_idx != UINT32_MAX) {
      auto& moved_cold = container_cold_pool_[moved.cold_idx];
      if (moved_cold.edge_target_id != UINT32_MAX) {
        edge_target_table_[moved_cold.edge_target_id].index = index;
      }
    }
  }
  vec.pop_back();
}

void Engine::ClearInstalledSubscriptions(ProcessHandle handle) {
  if (handle.process_id >= num_processes_) {
    return;
  }
  auto& proc_state = process_states_[handle.process_id];

  // Iterate in reverse so swap-and-pop doesn't invalidate earlier indices
  // belonging to this same process (they'll be removed too).
  for (auto it = proc_state.sub_refs.rbegin(); it != proc_state.sub_refs.rend();
       ++it) {
    switch (it->kind) {
      case SubKind::kEdge:
        RemoveEdgeSub(it->slot_id, it->index);
        break;
      case SubKind::kChange:
        RemoveChangeSub(it->slot_id, it->index);
        break;
      case SubKind::kRebindWatcher:
        RemoveRebindWatcherSub(it->slot_id, it->index);
        break;
      case SubKind::kContainer:
        RemoveContainerSub(it->slot_id, it->index);
        break;
    }
    if (live_subscription_count_ == 0) {
      throw common::InternalError(
          "Engine::ClearInstalledSubscriptions",
          "live_subscription_count_ underflow");
    }
    --live_subscription_count_;
  }

  proc_state.sub_refs.clear();
  proc_state.subscription_count = 0;
  proc_state.plan_pool.ops.clear();
}

void Engine::InvalidateInstalledWait(ProcessHandle handle) {
  if (handle.process_id >= num_processes_) return;
  auto& proc_state = process_states_[handle.process_id];
  proc_state.installed_wait = InstalledWaitState{};
}

void Engine::ResetInstalledWait(ProcessHandle handle) {
  ClearInstalledSubscriptions(handle);
  InvalidateInstalledWait(handle);
}

void Engine::ClearProcessSubscriptions(ProcessHandle handle) {
  ResetInstalledWait(handle);
}

void Engine::RefreshInstalledSnapshots(ProcessHandle handle) {
  if (handle.process_id >= num_processes_) return;

  auto& proc_state = process_states_[handle.process_id];

  std::span design_state(
      static_cast<const uint8_t*>(design_state_base_),
      slot_meta_registry_.MaxExtent());

  for (const auto& ref : proc_state.sub_refs) {
    const auto& meta = slot_meta_registry_.Get(ref.slot_id);

    switch (ref.kind) {
      case SubKind::kEdge: {
        auto& sub = signal_subs_[ref.slot_id].edge_subs[ref.index];
        uint8_t current_byte = design_state[meta.base_off + sub.byte_offset];
        sub.last_bit = (current_byte >> sub.bit_index) & 1;
        // Update byte snapshot if this is a rebind target.
        if (sub.cold_idx != UINT32_MAX) {
          auto& cold = edge_cold_pool_[sub.cold_idx];
          cold.edge_last_byte = current_byte;
          cold.has_edge_last_byte = true;
        }
        break;
      }
      case SubKind::kChange: {
        auto& sub = signal_subs_[ref.slot_id].change_subs[ref.index];
        const auto* current = &design_state[meta.base_off + sub.byte_offset];
        if (sub.byte_size <= ChangeSub::kInlineSnapshotCap) {
          std::memcpy(sub.snapshot_inline.data(), current, sub.byte_size);
        } else if (sub.cold_idx != UINT32_MAX) {
          auto& cold = change_cold_pool_[sub.cold_idx];
          if (cold.snapshot.size() < sub.byte_size) {
            cold.snapshot.resize(sub.byte_size);
          }
          std::memcpy(cold.snapshot.data(), current, sub.byte_size);
        }
        break;
      }
      case SubKind::kRebindWatcher: {
        // Structural invariant: RefreshInstalledSnapshots is only called when
        // can_refresh is true, which requires WaitShapeKind::kStatic. Static
        // waits have fixed trigger sets with no late-bound indices, so rebind
        // watchers are structurally impossible here.
        throw common::InternalError(
            "Engine::RefreshInstalledSnapshots",
            std::format(
                "process {} has rebind watcher on static refresh path",
                handle.process_id));
      }
      case SubKind::kContainer: {
        // Structural invariant: container element triggers require late-bound
        // index computation, so they are always WaitShapeKind::kDynamic and
        // never reach the static refresh path.
        throw common::InternalError(
            "Engine::RefreshInstalledSnapshots",
            std::format(
                "process {} has container sub on static refresh path",
                handle.process_id));
      }
    }
  }
}

void Engine::InstallTriggers(
    ProcessHandle handle, ResumePoint resume,
    std::span<const WaitTriggerRecord> triggers,
    std::span<const LateBoundHeader> late_bound,
    std::span<const IndexPlanOp> plan_ops,
    std::span<const uint32_t> dep_slots) {
  // Track created subscription info for late-bound rebinding.
  // Invariant: created_subs[i] records the dense vector index assigned
  // to trigger i at creation time. These indices remain stable within
  // this function because no removals occur between subscription
  // creation and rebind hookup below.
  struct CreatedSub {
    SignalId slot_id;
    SubKind kind;
    uint32_t index;
  };
  bool has_late_bound = !late_bound.empty();
  std::vector<CreatedSub> created_subs;
  if (has_late_bound) {
    created_subs.resize(
        triggers.size(), CreatedSub{0, SubKind::kEdge, UINT32_MAX});
  }

  for (uint32_t i = 0; i < triggers.size(); ++i) {
    const auto& trigger = triggers[i];
    auto edge = static_cast<common::EdgeKind>(trigger.edge);
    bool initially_active = (trigger.flags & kTriggerInitiallyActive) != 0;
    uint32_t sub_idx = UINT32_MAX;
    SubKind sub_kind = SubKind::kEdge;

    if (trigger.kind > static_cast<uint8_t>(TriggerInstallKind::kContainer)) {
      throw common::InternalError(
          "Engine::InstallTriggers",
          std::format("trigger {}: invalid kind {}", i, trigger.kind));
    }
    auto install_kind = static_cast<TriggerInstallKind>(trigger.kind);

    switch (install_kind) {
      case TriggerInstallKind::kContainer: {
        if (trigger.container_elem_stride == 0) {
          throw common::InternalError(
              "Engine::InstallTriggers",
              std::format(
                  "trigger {}: kContainer requires container_elem_stride > 0",
                  i));
        }
        if (trigger.byte_size != 0) {
          throw common::InternalError(
              "Engine::InstallTriggers",
              std::format(
                  "trigger {}: kContainer requires byte_size == 0, got {}", i,
                  trigger.byte_size));
        }
        if (trigger.bit_index != 0) {
          throw common::InternalError(
              "Engine::InstallTriggers",
              std::format(
                  "trigger {}: kContainer requires bit_index == 0, got {}", i,
                  trigger.bit_index));
        }
        if (edge != common::EdgeKind::kPosedge &&
            edge != common::EdgeKind::kNegedge &&
            edge != common::EdgeKind::kAnyChange) {
          throw common::InternalError(
              "Engine::InstallTriggers",
              std::format(
                  "trigger {}: kContainer has invalid edge {}", i,
                  static_cast<uint8_t>(edge)));
        }
        // For kContainer, byte_offset carries the element index directly.
        int64_t sv_index =
            initially_active ? static_cast<int64_t>(trigger.byte_offset) : -1;
        sub_idx = SubscribeContainerElement(
            handle, resume, trigger.signal_id, edge, sv_index,
            trigger.container_elem_stride, initially_active);
        sub_kind = SubKind::kContainer;
        break;
      }
      case TriggerInstallKind::kChange: {
        if (edge != common::EdgeKind::kAnyChange) {
          throw common::InternalError(
              "Engine::InstallTriggers",
              std::format(
                  "trigger {}: kChange requires kAnyChange edge, got {}", i,
                  static_cast<uint8_t>(edge)));
        }
        if (trigger.byte_size > 0) {
          sub_idx = Subscribe(
              handle, resume, trigger.signal_id, edge, trigger.byte_offset,
              trigger.byte_size, trigger.bit_index, initially_active);
        } else {
          sub_idx = Subscribe(
              handle, resume, trigger.signal_id, edge, initially_active);
        }
        sub_kind = SubKind::kChange;
        break;
      }
      case TriggerInstallKind::kEdge: {
        if (edge == common::EdgeKind::kAnyChange) {
          throw common::InternalError(
              "Engine::InstallTriggers",
              std::format(
                  "trigger {}: kEdge requires posedge/negedge, got kAnyChange",
                  i));
        }
        if (trigger.byte_size > 0) {
          sub_idx = Subscribe(
              handle, resume, trigger.signal_id, edge, trigger.byte_offset,
              trigger.byte_size, trigger.bit_index, initially_active);
        } else {
          sub_idx = Subscribe(
              handle, resume, trigger.signal_id, edge, initially_active);
        }
        sub_kind = SubKind::kEdge;
        break;
      }
    }

    if (has_late_bound) {
      created_subs[i] = CreatedSub{trigger.signal_id, sub_kind, sub_idx};
    }
  }

  // Install rebind watchers from late-bound headers.
  for (uint32_t h = 0; h < late_bound.size(); ++h) {
    const auto& hdr = late_bound[h];

    if (hdr.trigger_index >= triggers.size()) {
      throw common::InternalError(
          "Engine::InstallTriggers",
          std::format(
              "late_bound[{}]: trigger_index {} >= num_triggers {}", h,
              hdr.trigger_index, triggers.size()));
    }
    if (static_cast<uint64_t>(hdr.plan_ops_start) + hdr.plan_ops_count >
        plan_ops.size()) {
      throw common::InternalError(
          "Engine::InstallTriggers",
          std::format(
              "late_bound[{}]: plan_ops span [{}, +{}) exceeds pool size {}", h,
              hdr.plan_ops_start, hdr.plan_ops_count, plan_ops.size()));
    }
    if (static_cast<uint64_t>(hdr.dep_slots_start) + hdr.dep_slots_count >
        dep_slots.size()) {
      throw common::InternalError(
          "Engine::InstallTriggers",
          std::format(
              "late_bound[{}]: dep_slots span [{}, +{}) exceeds pool size {}",
              h, hdr.dep_slots_start, hdr.dep_slots_count, dep_slots.size()));
    }

    const auto& target = created_subs[hdr.trigger_index];
    if (target.index == UINT32_MAX) continue;
    if (hdr.dep_slots_count == 0) continue;

    BitTargetMapping mapping{
        .index_base = hdr.index_base,
        .index_step = hdr.index_step,
        .total_bits = hdr.total_bits};
    auto hdr_plan = plan_ops.subspan(hdr.plan_ops_start, hdr.plan_ops_count);
    auto hdr_deps = dep_slots.subspan(hdr.dep_slots_start, hdr.dep_slots_count);
    SubscribeRebind(
        handle, UINT32_MAX, target.slot_id, target.kind, target.index, hdr_plan,
        mapping, hdr_deps);
  }
}

void Engine::InstallWaitSite(
    ProcessHandle handle, SuspendRecord* suspend,
    const CompiledWaitSite& descriptor) {
  auto resume =
      ResumePoint{.block_index = suspend->resume_block, .instruction_index = 0};
  auto triggers = std::span(suspend->triggers_ptr, suspend->num_triggers);

  bool has_late_bound =
      suspend->num_late_bound > 0 && suspend->late_bound_ptr != nullptr;

  // Validate late-bound presence matches compiled descriptor.
  if (has_late_bound != descriptor.has_late_bound) {
    throw common::InternalError(
        "Engine::InstallWaitSite",
        std::format(
            "process {} wait_site {}: has_late_bound mismatch: "
            "descriptor={} vs suspend={}",
            handle.process_id, descriptor.id, descriptor.has_late_bound,
            has_late_bound));
  }

  auto late_bound =
      has_late_bound
          ? std::span(suspend->late_bound_ptr, suspend->num_late_bound)
          : std::span<const LateBoundHeader>{};
  auto plan_ops = (suspend->plan_ops_ptr != nullptr)
                      ? std::span(suspend->plan_ops_ptr, suspend->num_plan_ops)
                      : std::span<const IndexPlanOp>{};
  auto dep_slots =
      (suspend->dep_slots_ptr != nullptr)
          ? std::span(suspend->dep_slots_ptr, suspend->num_dep_slots)
          : std::span<const uint32_t>{};

  InstallTriggers(handle, resume, triggers, late_bound, plan_ops, dep_slots);

  // Unconditionally publish installed wait state from the compiled descriptor.
  auto& proc_state = process_states_[handle.process_id];
  proc_state.installed_wait = InstalledWaitState{
      .wait_site_id = descriptor.id, .shape = descriptor.shape, .valid = true};
}

void Engine::RegisterSuspendRecords(std::span<SuspendRecord*> records) {
  suspend_records_.assign(records.begin(), records.end());
}

void Engine::ReconcilePostActivation(ProcessHandle handle) {
  if (!HasPostActivationReconciliation()) {
    throw common::InternalError(
        "Engine::ReconcilePostActivation",
        "called without post-activation reconciliation capability");
  }
  if (handle.process_id >= suspend_records_.size()) {
    throw common::InternalError(
        "Engine::ReconcilePostActivation",
        std::format(
            "process_id {} >= suspend_records size {}", handle.process_id,
            suspend_records_.size()));
  }
  auto* suspend = suspend_records_[handle.process_id];

  auto resume =
      ResumePoint{.block_index = suspend->resume_block, .instruction_index = 0};

  switch (suspend->tag) {
    case SuspendTag::kFinished:
      ResetInstalledWait(handle);
      break;

    case SuspendTag::kDelay:
      ResetInstalledWait(handle);
      Delay(handle, resume, suspend->delay_ticks);
      break;

    case SuspendTag::kWait: {
      // Validate wait_site_id before use.
      if (suspend->wait_site_id == kInvalidWaitSiteId) {
        throw common::InternalError(
            "Engine::ReconcilePostActivation",
            std::format(
                "process {} suspended with kWait but wait_site_id is invalid",
                handle.process_id));
      }
      if (suspend->wait_site_id >= wait_site_meta_.Size()) {
        throw common::InternalError(
            "Engine::ReconcilePostActivation",
            std::format(
                "process {} wait_site_id {} >= registry size {}",
                handle.process_id, suspend->wait_site_id,
                wait_site_meta_.Size()));
      }

      const auto& descriptor = wait_site_meta_.Get(suspend->wait_site_id);

      // Validate transitional invariants: compiled descriptor must agree
      // with runtime activation record.
      if (descriptor.resume_block != suspend->resume_block) {
        throw common::InternalError(
            "Engine::ReconcilePostActivation",
            std::format(
                "process {} wait_site {} resume_block mismatch: "
                "descriptor={} vs suspend={}",
                handle.process_id, suspend->wait_site_id,
                descriptor.resume_block, suspend->resume_block));
      }
      if (descriptor.num_triggers != suspend->num_triggers) {
        throw common::InternalError(
            "Engine::ReconcilePostActivation",
            std::format(
                "process {} wait_site {} num_triggers mismatch: "
                "descriptor={} vs suspend={}",
                handle.process_id, suspend->wait_site_id,
                descriptor.num_triggers, suspend->num_triggers));
      }

      auto& proc_state = process_states_[handle.process_id];
      bool can_refresh =
          proc_state.installed_wait.valid &&
          proc_state.installed_wait.wait_site_id == suspend->wait_site_id &&
          descriptor.shape == WaitShapeKind::kStatic;

      if (can_refresh) {
        RefreshInstalledSnapshots(handle);
      } else {
        ResetInstalledWait(handle);
        InstallWaitSite(handle, suspend, descriptor);
      }
      break;
    }

    case SuspendTag::kRepeat:
      ResetInstalledWait(handle);
      ScheduleNextDelta(handle, ResumePoint{.block_index = 0});
      break;
  }
}

auto Engine::Subscribe(
    ProcessHandle handle, ResumePoint resume, SignalId signal,
    common::EdgeKind edge, bool initially_active) -> uint32_t {
  if (design_state_base_ == nullptr) {
    throw common::InternalError(
        "Engine::Subscribe", "Subscribe before SetDesignStateBase");
  }
  if (!slot_meta_registry_.IsPopulated()) {
    throw common::InternalError(
        "Engine::Subscribe", "Subscribe before InitSlotMeta");
  }

  const auto& meta = slot_meta_registry_.Get(signal);
  uint32_t obs_size =
      (edge == common::EdgeKind::kAnyChange) ? meta.total_bytes : 1;
  return Subscribe(
      handle, resume, signal, edge, 0, obs_size, 0, initially_active);
}

auto Engine::Subscribe(
    ProcessHandle handle, ResumePoint resume, SignalId signal,
    common::EdgeKind edge, uint32_t byte_offset, uint32_t byte_size,
    uint8_t bit_index, bool initially_active) -> uint32_t {
  if (finished_) {
    return UINT32_MAX;
  }

  if (design_state_base_ == nullptr) {
    throw common::InternalError(
        "Engine::Subscribe", "Subscribe before SetDesignStateBase");
  }
  if (!slot_meta_registry_.IsPopulated()) {
    throw common::InternalError(
        "Engine::Subscribe", "Subscribe before InitSlotMeta");
  }

  if (handle.process_id >= num_processes_) {
    throw common::InternalError(
        "Engine::Subscribe", std::format(
                                 "process_id {} exceeds num_processes {}",
                                 handle.process_id, num_processes_));
  }
  auto& proc_state = process_states_[handle.process_id];

  if (!CheckSubscriptionLimits(proc_state)) {
    return UINT32_MAX;
  }

  const auto& meta = slot_meta_registry_.Get(signal);

  // Range validation.
  if (byte_size == 0) {
    throw common::InternalError("Engine::Subscribe", "byte_size must be > 0");
  }
  if (static_cast<uint64_t>(byte_offset) + static_cast<uint64_t>(byte_size) >
      meta.total_bytes) {
    throw common::InternalError(
        "Engine::Subscribe", "observation range exceeds slot size");
  }

  std::span design_state(
      static_cast<const uint8_t*>(design_state_base_),
      slot_meta_registry_.MaxExtent());

  auto& slot = signal_subs_[signal];

  if (edge == common::EdgeKind::kAnyChange) {
    // kAnyChange -> ChangeSub
    // Baseline capture policy: always capture snapshot regardless of
    // initially_active. When a rebind later activates an inactive sub,
    // the baseline prevents a false trigger on the first flush.
    auto sub_idx = static_cast<uint32_t>(slot.change_subs.size());
    auto proc_sub_idx = static_cast<uint32_t>(proc_state.sub_refs.size());

    ChangeSub sub{};
    sub.process_id = handle.process_id;
    sub.instance_id = handle.instance_id;
    sub.resume_block = resume.block_index;
    sub.byte_offset = byte_offset;
    sub.byte_size = byte_size;
    sub.process_sub_idx = proc_sub_idx;
    sub.cold_idx = UINT32_MAX;
    sub.flags = initially_active ? kSubActive : 0;

    const auto* src = &design_state[meta.base_off + byte_offset];
    if (byte_size <= ChangeSub::kInlineSnapshotCap) {
      std::memcpy(sub.snapshot_inline.data(), src, byte_size);
    } else {
      // Large snapshot -> need cold entry.
      sub.cold_idx = AllocChangeCold();
      auto& cold = change_cold_pool_[sub.cold_idx];
      cold.snapshot.resize(byte_size);
      std::memcpy(cold.snapshot.data(), src, byte_size);
    }

    slot.change_subs.push_back(sub);
    proc_state.sub_refs.push_back(
        SubRef{.slot_id = signal, .index = sub_idx, .kind = SubKind::kChange});
    ++proc_state.subscription_count;
    ++live_subscription_count_;
    return sub_idx;
  }

  // kPosedge/kNegedge -> EdgeSub
  // Baseline capture policy: always capture last_bit regardless of
  // initially_active. When a rebind later activates an inactive sub,
  // the baseline prevents a false trigger on the first flush.
  if (byte_size != 1) {
    throw common::InternalError(
        "Engine::Subscribe", "edge subscriptions require byte_size=1");
  }
  if (bit_index > 7) {
    throw common::InternalError(
        "Engine::Subscribe", "bit_index must be in [0,7]");
  }

  auto sub_idx = static_cast<uint32_t>(slot.edge_subs.size());
  auto proc_sub_idx = static_cast<uint32_t>(proc_state.sub_refs.size());

  EdgeSub sub{};
  sub.process_id = handle.process_id;
  sub.instance_id = handle.instance_id;
  sub.resume_block = resume.block_index;
  sub.byte_offset = byte_offset;
  sub.byte_size = byte_size;
  sub.bit_index = bit_index;
  sub.edge = edge;
  sub.last_bit = (design_state[meta.base_off + byte_offset] >> bit_index) & 1;
  sub.flags = initially_active ? kSubActive : 0;
  sub.process_sub_idx = proc_sub_idx;
  sub.cold_idx = UINT32_MAX;

  slot.edge_subs.push_back(sub);
  proc_state.sub_refs.push_back(
      SubRef{.slot_id = signal, .index = sub_idx, .kind = SubKind::kEdge});
  ++proc_state.subscription_count;
  ++live_subscription_count_;
  return sub_idx;
}

auto Engine::SubscribeContainerElement(
    ProcessHandle handle, ResumePoint resume, SignalId signal,
    common::EdgeKind edge, int64_t sv_index, uint32_t elem_stride,
    bool initially_active) -> uint32_t {
  if (finished_) return UINT32_MAX;

  if (design_state_base_ == nullptr) {
    throw common::InternalError(
        "Engine::SubscribeContainerElement",
        "SubscribeContainerElement before SetDesignStateBase");
  }
  if (!slot_meta_registry_.IsPopulated()) {
    throw common::InternalError(
        "Engine::SubscribeContainerElement",
        "SubscribeContainerElement before InitSlotMeta");
  }

  if (handle.process_id >= num_processes_) {
    throw common::InternalError(
        "Engine::SubscribeContainerElement",
        std::format(
            "process_id {} exceeds num_processes {}", handle.process_id,
            num_processes_));
  }
  if (elem_stride == 0) {
    throw common::InternalError(
        "Engine::SubscribeContainerElement", "elem_stride must be > 0");
  }

  auto& proc_state = process_states_[handle.process_id];
  if (!CheckSubscriptionLimits(proc_state)) return UINT32_MAX;

  const auto& meta = slot_meta_registry_.Get(signal);
  auto& slot = signal_subs_[signal];

  auto sub_idx = static_cast<uint32_t>(slot.container_subs.size());
  auto proc_sub_idx = static_cast<uint32_t>(proc_state.sub_refs.size());

  // Always allocate cold entry for container subs.
  uint32_t cold_idx = AllocContainerCold();
  auto& cold = container_cold_pool_[cold_idx];
  cold.container_base_off = meta.base_off;
  cold.container_elem_stride = elem_stride;
  cold.container_sv_index = sv_index;

  ContainerSub sub{};
  sub.process_id = handle.process_id;
  sub.instance_id = handle.instance_id;
  sub.resume_block = resume.block_index;
  sub.process_sub_idx = proc_sub_idx;
  sub.cold_idx = cold_idx;
  sub.edge = edge;
  sub.last_bit = 0;
  sub.flags = 0;

  // Chase handle from DesignState.
  auto ds = std::span(
      static_cast<const uint8_t*>(design_state_base_),
      meta.base_off + sizeof(void*));
  void* handle_ptr = nullptr;
  std::memcpy(&handle_ptr, &ds[meta.base_off], sizeof(void*));

  if (!initially_active) {
    // Descriptor says inactive -- skip runtime validation entirely.
    cold.container_epoch = 0;
  } else if (handle_ptr == nullptr) {
    cold.container_epoch = 0;
  } else {
    const auto* arr = static_cast<const DynArrayData*>(handle_ptr);
    if (arr->magic != DynArrayData::kMagic) {
      throw common::InternalError(
          "Engine::SubscribeContainerElement", "invalid container magic");
    }
    cold.container_epoch = arr->epoch;

    if (arr->data != nullptr && sv_index >= 0 && sv_index < arr->size) {
      // Capture initial LSB of element byte 0 for edge detection.
      // Container edge triggers observe bit 0 of the first byte of each
      // element (byte_off = sv_index * elem_stride). This matches the
      // scalar edge semantics: @(posedge d[i]) triggers on the LSB.
      auto byte_off = static_cast<uint32_t>(sv_index) * elem_stride;
      auto heap_data = std::span(
          static_cast<const uint8_t*>(arr->data), byte_off + elem_stride);
      sub.last_bit = heap_data[byte_off] & 1;
      sub.flags = kSubActive;
    }
  }

  slot.container_subs.push_back(sub);
  proc_state.sub_refs.push_back(
      SubRef{.slot_id = signal, .index = sub_idx, .kind = SubKind::kContainer});
  ++proc_state.subscription_count;
  ++live_subscription_count_;
  return sub_idx;
}

void Engine::SubscribeRebind(
    ProcessHandle handle, uint32_t edge_target_id, SignalId target_slot,
    SubKind target_kind, uint32_t target_index,
    std::span<const IndexPlanOp> plan, BitTargetMapping mapping,
    std::span<const uint32_t> dep_slots) {
  if (finished_) return;

  if (design_state_base_ == nullptr) {
    throw common::InternalError(
        "Engine::SubscribeRebind", "SubscribeRebind before SetDesignStateBase");
  }
  if (!slot_meta_registry_.IsPopulated()) {
    throw common::InternalError(
        "Engine::SubscribeRebind", "SubscribeRebind before InitSlotMeta");
  }

  if (handle.process_id >= num_processes_) {
    throw common::InternalError(
        "Engine::SubscribeRebind", std::format(
                                       "process_id {} exceeds num_processes {}",
                                       handle.process_id, num_processes_));
  }
  if (target_kind != SubKind::kEdge && target_kind != SubKind::kContainer) {
    throw common::InternalError(
        "Engine::SubscribeRebind",
        std::format(
            "rebind target kind {} not supported (must be kEdge or kContainer)",
            static_cast<int>(target_kind)));
  }

  // Validate target_slot and target_index against current dense storage.
  if (target_slot >= signal_subs_.size()) {
    throw common::InternalError(
        "Engine::SubscribeRebind", std::format(
                                       "target_slot {} >= signal_subs size {}",
                                       target_slot, signal_subs_.size()));
  }
  if (target_kind == SubKind::kEdge) {
    if (target_index >= signal_subs_[target_slot].edge_subs.size()) {
      throw common::InternalError(
          "Engine::SubscribeRebind",
          std::format(
              "target_index {} >= edge_subs size {} for slot {}", target_index,
              signal_subs_[target_slot].edge_subs.size(), target_slot));
    }
  } else {
    if (target_index >= signal_subs_[target_slot].container_subs.size()) {
      throw common::InternalError(
          "Engine::SubscribeRebind",
          std::format(
              "target_index {} >= container_subs size {} for slot {}",
              target_index, signal_subs_[target_slot].container_subs.size(),
              target_slot));
    }
  }

  auto& proc_state = process_states_[handle.process_id];

  // Pre-validate capacity for ALL dep_slot watchers before mutating state.
  // This ensures atomicity: either all watchers install or none do.
  auto total_after = proc_state.subscription_count + dep_slots.size();
  auto global_after = live_subscription_count_ + dep_slots.size();
  if (max_total_subscriptions_ > 0 && global_after > max_total_subscriptions_) {
    TerminateWithResourceError(
        "global subscription limit exceeded (rebind batch)",
        live_subscription_count_, max_total_subscriptions_);
    return;
  }
  if (max_subscriptions_per_process_ > 0 &&
      total_after > max_subscriptions_per_process_) {
    TerminateWithResourceError(
        "per-process subscription limit exceeded (rebind batch)",
        proc_state.subscription_count, max_subscriptions_per_process_);
    return;
  }

  // Store plan in process's pool.
  auto plan_start = static_cast<uint32_t>(proc_state.plan_pool.ops.size());
  proc_state.plan_pool.ops.insert(
      proc_state.plan_pool.ops.end(), plan.begin(), plan.end());
  IndexPlanRef plan_ref = {
      .start = plan_start, .count = static_cast<uint16_t>(plan.size())};

  // Ensure the target has a cold entry and store plan/mapping.
  if (target_kind == SubKind::kEdge) {
    auto& esub = signal_subs_[target_slot].edge_subs[target_index];
    if (esub.cold_idx == UINT32_MAX) {
      esub.cold_idx = AllocEdgeCold();
      esub.flags |= kSubHasCold;
      // Seed byte snapshot for same-byte rebinding detection.
      auto& new_cold = edge_cold_pool_[esub.cold_idx];
      const auto& tmeta = slot_meta_registry_.Get(target_slot);
      std::span ds(
          static_cast<const uint8_t*>(design_state_base_),
          tmeta.base_off + esub.byte_offset + 1);
      new_cold.edge_last_byte = ds[tmeta.base_off + esub.byte_offset];
      new_cold.has_edge_last_byte = true;
    }
    auto& ecold = edge_cold_pool_[esub.cold_idx];
    ecold.plan_ref = plan_ref;
    ecold.rebind_mapping = mapping;
    // Allocate edge target handle if not provided (legacy path).
    if (edge_target_id == UINT32_MAX) {
      edge_target_id = AllocEdgeTarget(
          EdgeTargetHandle{target_slot, SubKind::kEdge, target_index});
      ecold.edge_target_id = edge_target_id;
    }
  } else {
    // kContainer
    auto& csub = signal_subs_[target_slot].container_subs[target_index];
    auto& ccold = container_cold_pool_[csub.cold_idx];
    ccold.plan_ref = plan_ref;
    ccold.rebind_mapping = mapping;
    // Allocate edge target handle if not provided (legacy path).
    if (edge_target_id == UINT32_MAX) {
      edge_target_id = AllocEdgeTarget(
          EdgeTargetHandle{target_slot, SubKind::kContainer, target_index});
      ccold.edge_target_id = edge_target_id;
    }
  }

  // Create a rebind watcher for each dep slot.
  // Capacity was pre-validated above, so no per-watcher limit check needed.
  for (uint32_t dep_slot : dep_slots) {
    const auto& meta = slot_meta_registry_.Get(dep_slot);
    auto& slot = signal_subs_[dep_slot];

    auto watcher_idx = static_cast<uint32_t>(slot.rebind_subs.size());
    auto watcher_proc_idx = static_cast<uint32_t>(proc_state.sub_refs.size());

    // Always allocate cold entry for rebind watchers.
    uint32_t watcher_cold = AllocWatcherCold();
    auto& wcold = watcher_cold_pool_[watcher_cold];
    wcold.edge_target_id = edge_target_id;

    // Capture initial snapshot of dep slot.
    std::span design_state(
        static_cast<const uint8_t*>(design_state_base_),
        slot_meta_registry_.MaxExtent());
    const auto* src = &design_state[meta.base_off];
    wcold.snapshot.resize(meta.total_bytes);
    std::memcpy(wcold.snapshot.data(), src, meta.total_bytes);

    RebindWatcherSub watcher{};
    watcher.process_id = handle.process_id;
    watcher.byte_offset = 0;
    watcher.byte_size = meta.total_bytes;
    watcher.process_sub_idx = watcher_proc_idx;
    watcher.cold_idx = watcher_cold;
    watcher.flags = kSubActive;

    slot.rebind_subs.push_back(watcher);
    proc_state.sub_refs.push_back(
        SubRef{
            .slot_id = dep_slot,
            .index = watcher_idx,
            .kind = SubKind::kRebindWatcher});
    ++proc_state.subscription_count;
    ++live_subscription_count_;
  }

  // Initial rebind: validate codegen-computed target against runtime state.
  RebindSubscription(edge_target_id);
}

void Engine::RebindSubscription(uint32_t edge_target_id) {
  if (edge_target_id >= edge_target_table_.size()) {
    throw common::InternalError(
        "Engine::RebindSubscription",
        std::format(
            "edge_target_id {} >= table size {}", edge_target_id,
            edge_target_table_.size()));
  }
  auto& target_handle = edge_target_table_[edge_target_id];
  if (target_handle.slot_id == UINT32_MAX) return;  // freed

  // Resolve plan/mapping and epoch from the appropriate cold pool.
  IndexPlanRef plan_ref;
  BitTargetMapping mapping;
  uint32_t* epoch_ptr = nullptr;
  uint32_t target_process_id = 0;

  if (target_handle.kind == SubKind::kEdge) {
    auto& esub =
        signal_subs_[target_handle.slot_id].edge_subs[target_handle.index];
    if (esub.cold_idx == UINT32_MAX) return;
    auto& ecold = edge_cold_pool_[esub.cold_idx];
    plan_ref = ecold.plan_ref;
    mapping = ecold.rebind_mapping;
    epoch_ptr = &ecold.last_rebind_epoch;
    target_process_id = esub.process_id;
  } else {
    auto& csub =
        signal_subs_[target_handle.slot_id].container_subs[target_handle.index];
    auto& ccold = container_cold_pool_[csub.cold_idx];
    plan_ref = ccold.plan_ref;
    mapping = ccold.rebind_mapping;
    epoch_ptr = &ccold.last_rebind_epoch;
    target_process_id = csub.process_id;
  }

  // Epoch guard: skip if already rebound this flush cycle.
  if (*epoch_ptr == flush_epoch_) return;
  *epoch_ptr = flush_epoch_;

  // Resolve plan from the process's pool.
  if (target_process_id >= num_processes_) {
    throw common::InternalError(
        "Engine::RebindSubscription",
        std::format(
            "process_id {} exceeds num_processes {}", target_process_id,
            num_processes_));
  }
  auto& proc_state = process_states_[target_process_id];
  auto all_ops = std::span<const IndexPlanOp>(proc_state.plan_pool.ops);
  auto plan_span = all_ops.subspan(plan_ref.start, plan_ref.count);

  bool should_deactivate = false;
  int64_t index_val = EvaluateIndexPlan(
      design_state_base_, slot_meta_registry_, plan_span, &should_deactivate);

  // Helper lambdas for active/inactive flag setting.
  auto set_inactive = [&]() {
    if (target_handle.kind == SubKind::kEdge) {
      signal_subs_[target_handle.slot_id]
          .edge_subs[target_handle.index]
          .flags &= ~kSubActive;
    } else {
      signal_subs_[target_handle.slot_id]
          .container_subs[target_handle.index]
          .flags &= ~kSubActive;
    }
  };

  if (should_deactivate) {
    set_inactive();
    return;
  }

  // Container rebind path: recompute which element is observed.
  // After index plan evaluation, we update container_sv_index to the new
  // element index, chase the heap handle to validate bounds, and re-seed
  // last_bit from the new element's LSB. This is sufficient because container
  // edge triggers observe only bit 0 of byte 0 of the element (see
  // ContainerSub observation model). The next FlushContainerSub will compare
  // the reseeded last_bit against the current element value.
  if (target_handle.kind == SubKind::kContainer) {
    auto& csub =
        signal_subs_[target_handle.slot_id].container_subs[target_handle.index];
    auto& ccold = container_cold_pool_[csub.cold_idx];

    auto ds = std::span(
        static_cast<const uint8_t*>(design_state_base_),
        ccold.container_base_off + sizeof(void*));
    void* handle_ptr = nullptr;
    std::memcpy(&handle_ptr, &ds[ccold.container_base_off], sizeof(void*));

    if (handle_ptr == nullptr) {
      set_inactive();
      return;
    }
    const auto* arr = static_cast<const DynArrayData*>(handle_ptr);
    if (arr->magic != DynArrayData::kMagic) {
      throw common::InternalError(
          "Engine::RebindSubscription", "invalid container magic");
    }
    ccold.container_sv_index = index_val;
    ccold.container_epoch = arr->epoch;
    if (arr->data == nullptr || index_val < 0 || index_val >= arr->size) {
      set_inactive();
      return;
    }
    csub.flags |= kSubActive;
    // Capture initial bit for edge detection.
    auto byte_off =
        static_cast<uint32_t>(index_val) * ccold.container_elem_stride;
    auto heap_data =
        std::span(static_cast<const uint8_t*>(arr->data), byte_off + 1);
    csub.last_bit = heap_data[byte_off] & 1;
    return;
  }

  // Edge path: packed/unpacked DesignState bit position update.
  int64_t logical_offset =
      static_cast<int64_t>(index_val - mapping.index_base) * mapping.index_step;

  if (logical_offset < 0 ||
      static_cast<uint64_t>(logical_offset) >= mapping.total_bits) {
    set_inactive();
    return;
  }

  auto& esub =
      signal_subs_[target_handle.slot_id].edge_subs[target_handle.index];
  esub.flags |= kSubActive;
  auto bit = static_cast<uint64_t>(logical_offset);
  auto new_byte_offset = static_cast<uint32_t>(bit / 8);
  auto new_bit_index = static_cast<uint8_t>(bit % 8);

  // Update the target's observation position.
  auto& ecold = edge_cold_pool_[esub.cold_idx];
  const auto& meta = slot_meta_registry_.Get(target_handle.slot_id);
  auto ds = std::span(
      static_cast<const uint8_t*>(design_state_base_),
      meta.base_off + new_byte_offset + 1);

  // Same-byte rebinding: extract old bit from the saved byte snapshot
  // so the edge pass detects the 0->1 / 1->0 transition correctly.
  // Different-byte: read from current design state (no prior observation).
  if (new_byte_offset == esub.byte_offset && ecold.has_edge_last_byte) {
    esub.last_bit = (ecold.edge_last_byte >> new_bit_index) & 1;
  } else {
    esub.last_bit = (ds[meta.base_off + new_byte_offset] >> new_bit_index) & 1;
  }
  // Save full byte snapshot for potential future same-byte rebinding.
  ecold.edge_last_byte = ds[meta.base_off + new_byte_offset];
  ecold.has_edge_last_byte = true;
  esub.byte_offset = new_byte_offset;
  esub.bit_index = new_bit_index;
}

void Engine::FlushContainerSub(
    ContainerSub& sub, std::span<const uint8_t> design_state) {
  auto& cold = container_cold_pool_[sub.cold_idx];

  // Chase handle from DesignState.
  void* handle_ptr = nullptr;
  std::memcpy(
      &handle_ptr, &design_state[cold.container_base_off], sizeof(void*));

  if (handle_ptr == nullptr) {
    sub.flags &= ~kSubActive;
    return;
  }

  const auto* arr = static_cast<const DynArrayData*>(handle_ptr);
  if (arr->magic != DynArrayData::kMagic) {
    throw common::InternalError(
        "Engine::FlushContainerSub", "invalid container magic");
  }

  int64_t idx = cold.container_sv_index;

  if (arr->data == nullptr || idx < 0 || idx >= arr->size) {
    sub.flags &= ~kSubActive;
    cold.container_epoch = arr->epoch;
    return;
  }

  // Element is in range.
  if (!(sub.flags & kSubActive)) {
    // Was inactive, now in-range: reactivate, capture snapshot, no edge.
    sub.flags |= kSubActive;
    cold.container_epoch = arr->epoch;
    auto byte_off = static_cast<uint32_t>(idx) * cold.container_elem_stride;
    auto heap_data =
        std::span(static_cast<const uint8_t*>(arr->data), byte_off + 1);
    sub.last_bit = heap_data[byte_off] & 1;
    return;
  }

  cold.container_epoch = arr->epoch;

  auto byte_off = static_cast<uint32_t>(idx) * cold.container_elem_stride;
  auto heap_data = std::span(
      static_cast<const uint8_t*>(arr->data),
      byte_off + cold.container_elem_stride);
  uint8_t current_bit = heap_data[byte_off] & 1;

  bool should_wake =
      EvaluateEdge(sub.edge, sub.last_bit != 0, current_bit != 0);
  sub.last_bit = current_bit;

  if (should_wake) {
    if (sub.process_id >= num_processes_) {
      throw common::InternalError(
          "Engine::FlushContainerSub",
          std::format(
              "wakeup process_id {} exceeds num_processes {}", sub.process_id,
              num_processes_));
    }
    auto& proc_state = process_states_[sub.process_id];
    if (!proc_state.is_enqueued) {
      next_delta_queue_.push_back(
          ScheduledEvent{
              .handle = ProcessHandle{sub.process_id, sub.instance_id},
              .resume = ResumePoint{sub.resume_block, 0}});
      proc_state.is_enqueued = true;
    }
  }
}

void Engine::FlushSignalUpdates() {
  if (finished_) {
    return;
  }

  // Guard: MIR path has no slot meta / design state.
  if (!slot_meta_registry_.IsPopulated() || design_state_base_ == nullptr) {
    return;
  }

  auto newly_dirty = update_set_.DeltaDirtySlots();
  if (newly_dirty.empty()) {
    return;
  }

  // Increment flush epoch for rebind deduplication.
  ++flush_epoch_;

  std::span design_state(
      static_cast<const uint8_t*>(design_state_base_),
      slot_meta_registry_.MaxExtent());

  // Pass 1: Rebind phase -- iterate dense rebind_subs.
  for (uint32_t slot_id : newly_dirty) {
    auto& slot = signal_subs_[slot_id];
    if (slot.rebind_subs.empty()) continue;

    const auto& meta = slot_meta_registry_.Get(slot_id);

    for (auto& sub : slot.rebind_subs) {
      auto& wcold = watcher_cold_pool_[sub.cold_idx];
      const auto* current = &design_state[meta.base_off + sub.byte_offset];
      auto* snapshot = wcold.snapshot.data();
      if (std::memcmp(current, snapshot, sub.byte_size) != 0) {
        std::memcpy(snapshot, current, sub.byte_size);
        RebindSubscription(wcold.edge_target_id);
      }
    }
  }

  // Pass 2a: Edge phase -- iterate dense edge_subs.
  for (uint32_t slot_id : newly_dirty) {
    auto& edge_subs = signal_subs_[slot_id].edge_subs;
    if (edge_subs.empty()) continue;
    const auto& dirty_ranges = update_set_.DeltaRangesFor(slot_id);
    const auto& meta = slot_meta_registry_.Get(slot_id);

    for (auto& sub : edge_subs) {
      if (!(sub.flags & kSubActive)) continue;
      if (!dirty_ranges.Overlaps(sub.byte_offset, sub.byte_size)) continue;

      uint8_t current_byte = design_state[meta.base_off + sub.byte_offset];
      uint8_t current_bit = (current_byte >> sub.bit_index) & 1;

      bool should_wake = false;
      if (sub.last_bit != current_bit) {
        should_wake =
            EvaluateEdge(sub.edge, sub.last_bit != 0, current_bit != 0);
        sub.last_bit = current_bit;
      }
      // Update byte snapshot for potential same-byte rebinding.
      if (sub.cold_idx != UINT32_MAX) {
        auto& ecold = edge_cold_pool_[sub.cold_idx];
        ecold.edge_last_byte = current_byte;
        ecold.has_edge_last_byte = true;
      }

      if (should_wake) {
        auto& proc_state = process_states_[sub.process_id];
        if (!proc_state.is_enqueued) {
          next_delta_queue_.push_back(
              ScheduledEvent{
                  .handle = ProcessHandle{sub.process_id, sub.instance_id},
                  .resume = ResumePoint{sub.resume_block, 0}});
          proc_state.is_enqueued = true;
        }
      }
    }
  }

  // Pass 2b: Change phase -- iterate dense change_subs.
  for (uint32_t slot_id : newly_dirty) {
    auto& change_subs = signal_subs_[slot_id].change_subs;
    if (change_subs.empty()) continue;
    const auto& dirty_ranges = update_set_.DeltaRangesFor(slot_id);
    const auto& meta = slot_meta_registry_.Get(slot_id);

    for (auto& sub : change_subs) {
      if (!(sub.flags & kSubActive)) continue;
      if (!dirty_ranges.Overlaps(sub.byte_offset, sub.byte_size)) continue;

      const auto* current = &design_state[meta.base_off + sub.byte_offset];
      uint8_t* snapshot = nullptr;
      if (sub.byte_size <= ChangeSub::kInlineSnapshotCap) {
        snapshot = sub.snapshot_inline.data();
      } else if (sub.cold_idx != UINT32_MAX) {
        snapshot = change_cold_pool_[sub.cold_idx].snapshot.data();
      }
      if (snapshot == nullptr) continue;

      if (std::memcmp(current, snapshot, sub.byte_size) != 0) {
        std::memcpy(snapshot, current, sub.byte_size);
        auto& proc_state = process_states_[sub.process_id];
        if (!proc_state.is_enqueued) {
          next_delta_queue_.push_back(
              ScheduledEvent{
                  .handle = ProcessHandle{sub.process_id, sub.instance_id},
                  .resume = ResumePoint{sub.resume_block, 0}});
          proc_state.is_enqueued = true;
        }
      }
    }
  }

  // Pass 2c: Container phase -- iterate dense container_subs.
  // No delta-range filtering: container subs observe heap-allocated data
  // whose byte offsets are not tracked by the slot-level dirty ranges.
  // The slot being dirty is the only signal; FlushContainerSub chases the
  // heap handle and compares element values directly. This is intentionally
  // unconditional -- container subs are rare and the per-sub cost is low.
  for (uint32_t slot_id : newly_dirty) {
    auto& container_subs = signal_subs_[slot_id].container_subs;
    if (container_subs.empty()) continue;

    for (auto& sub : container_subs) {
      FlushContainerSub(sub, design_state);
    }
  }
}

}  // namespace lyra::runtime
